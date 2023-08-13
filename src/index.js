const readline = require('readline');
const { spawn } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawnSync } = require('child_process');

const WebServer = require('./utils/configManager')

const events = require('events');
const eventEmitter = new events.EventEmitter();

const promptPlaceholders = {
  '%u': os.userInfo().username,
  '%h': os.hostname(),
  '%i': 'IP_ADDRESS', // Replace this with actual code to get IP address
  '%d': path.basename(process.cwd()),
  '%t': new Date().toLocaleTimeString(),
  '%%': '%',
  '%c': process.cwd(),
  '%C': path.basename(process.cwd()),
  '%B': '\x1b[1m',  // Bold text
  '%b': '\x1b[0m',  // Reset text formatting
  '%n': '\n'
};

const updatePromptPlaceholders = () => {
  promptPlaceholders['%c'] = process.cwd();
  promptPlaceholders['%C'] = path.basename(process.cwd());
};

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: 'haksh> ',
  historySize: 100,
});

const aliases = {};

const formatPrompt = (customPrompt) => {
  updatePromptPlaceholders();
  let formattedPrompt = customPrompt;
  for (const placeholder in promptPlaceholders) {
    formattedPrompt = formattedPrompt.replace(new RegExp(placeholder, 'g'), promptPlaceholders[placeholder]);
  }

  // Add color formatting using ANSI escape codes
  formattedPrompt = formattedPrompt.replace(/\\e\[(\d+)(?::(\d+))?m/g, (_, code, value) => {
    if (value) {
      return `\x1b[${code};${value}m`;
    }
    return `\x1b[${code}m`;
  });

  return formattedPrompt + '\x1b[0m'; // Reset color formatting at the end
};

const pluginDir =  path.join(os.homedir(), `.haksh/plugins`);
const loadedPlugins = [];

if (!fs.existsSync(pluginDir)) {
  fs.mkdirSync(pluginDir, { recursive: true });
  console.log(`Plugin directory created: ${pluginDir}`);
}

fs.readdirSync(pluginDir).forEach(pluginName => {
  const pluginPath = path.join(pluginDir, pluginName, 'index.js');
  if (fs.existsSync(pluginPath)) {
    const plugin = require(pluginPath);
    loadedPlugins.push({ name: pluginName, module: plugin });
    // console.log(`Loaded plugin: ${pluginName}`);

    if (typeof plugin.applyToPlaceholders === 'function') {
      plugin.applyToPlaceholders(promptPlaceholders);
    }

    if (typeof plugin.onCommandExecuted === 'function') {
      eventEmitter.on('commandExecuted', command => {
        plugin.onCommandExecuted(command);
      });
    }

    if (typeof plugin.applyToPlaceholdersOnUpdate === 'function') {
      eventEmitter.on('commandExecuted', command => {
        const updateFunction = plugin.applyToPlaceholdersOnUpdate(promptPlaceholders);
        updateFunction();
      });
    }


  }
});


const historyFilePath = path.join(os.homedir(), '.haksh_history');
let commandHistory = [];

let customPrompt = '';

const rcFilePath = path.join(os.homedir(), '.hakshrc');

try {
  const fileContents = fs.readFileSync(historyFilePath, 'utf8');
  commandHistory = fileContents.split('\n').filter(line => line.trim() !== '');
} catch (error) {
  console.log(error)
}

let commandIndex = -1;

const loadAliasesFromRc = () => {
  try {
    const rcContent = fs.readFileSync(rcFilePath, 'utf8');
    const lines = rcContent.split('\n');

    const commandsToRun = [];
    lines.forEach((line) => {
      if (line.trim().startsWith('alias')) {
        const [, aliasName, aliasValue] = line.match(/alias\s+(\w+)\s*=\s*(.+)/);
        aliases[aliasName] = aliasValue.replace(/['"]/g, ''); // Remove ' or " characters
      } else if (line.trim().startsWith('PS1=')) {
        customPrompt = line.trim().substring(4).replace(/['"]/g, '');;
      } else if (line.trim() !== '') {
        commandsToRun.push(line.trim());
      }
    });

    if (commandsToRun.length > 0) {
      commandsToRun.forEach((command) => {
        const result = spawnSync(command, { shell: true, stdio: 'inherit' });
        if (result.error) {
          console.error(`Error executing '${command}': ${result.error.message}`);
        }
      });
    }

    if (customPrompt) {
      // Set the custom prompt
      rl.setPrompt(formatPrompt(customPrompt));
    }
  } catch (error) {
    // Ignore errors if the file doesn't exist
  }
    setTimeout(() => {
    rl.prompt(); // Display the prompt after executing commands
  }, 0);
};

loadAliasesFromRc(); // Load aliases when the shell starts

const webServer = new WebServer(rcFilePath);

rl.on('line', (line) => {
  rl.setPrompt(formatPrompt(customPrompt));

  line = expandAliases(line);
  const pipelineCommands = line.split('|').map(cmd => cmd.trim());

  if (line.trim() !== '') {
    commandHistory.push(line);
    if (commandHistory.length > rl.historySize) {
      commandHistory.shift();
    }
    commandIndex = -1;
  }

  // If a pipeline is detected
  if (pipelineCommands.length > 1) {
    let input = null;

    pipelineCommands.forEach((command, index) => {
      const args = command.split(' ');
      const commandName = args[0];
      const arguments = args.slice(1);

      const child = spawn(commandName, arguments, {
        stdio: [input || 'inherit', 'pipe', process.stderr] // Pipe stdin and stdout
      });

      if (input) {
        input.pipe(child.stdin);
      }

      if (index === pipelineCommands.length - 1) {
        // Last command in pipeline, pipe output to process.stdout
        child.stdout.pipe(process.stdout);
        child.on('close', () => {
          rl.prompt();
        });
      }

      input = child.stdout;
    });
  } else {
    const args = line.split(' ');
    const command = args[0];
    const arguments = args.slice(1);

    if (command === 'cd') {
      if (arguments.length === 0) {
        console.log('Usage: cd <directory>');
      } else {
        try {
          process.chdir(arguments[0]);
          eventEmitter.emit('commandExecuted', line);
          updatePromptPlaceholders();
          rl.setPrompt(formatPrompt(customPrompt));
        } catch (error) {
          console.error(`cd: ${error.message}`);
        }
      }
      rl.prompt();
    } else if (command === 'alias') {
  if (arguments.length === 0) {
    // Display list of aliases
    for (const alias in aliases) {
      console.log(`${alias}='${aliases[alias]}'`);
    }
  } else {
    const aliasName = arguments[0];
    const aliasValue = arguments.slice(1).join(' ');
    aliases[aliasName] = aliasValue;
  }
  rl.prompt();
} else if (command === 'hakshconfig') {
        if (arguments.length === 0) {
        console.log('Usage: hakshconfig start | quit');
      } else {
        try {
          if (arguments[0] === 'start') {
            webServer.start(8080)
          }
          if (arguments[0] === 'quit') {
            webServer.stop()
          }
        } catch (error) {
          console.error(`${error.message}`);
        }
      }
  rl.prompt();
} else {
      const outputFileIndex = arguments.indexOf('>');
      if (outputFileIndex !== -1) {
        const outputFileName = arguments[outputFileIndex + 1];
        arguments.splice(outputFileIndex, 2); // Remove '>' and the output file name
        const child = spawn(command, arguments);

        const outputStream = fs.createWriteStream(outputFileName, { flags: 'a' });
        child.stdout.pipe(outputStream);

        child.on('close', () => {
          rl.prompt();
        });
      } else {
        const child = spawn(command, arguments, { stdio: 'inherit' });

        child.on('error', (error) => {
          console.error(`Error executing ${command}: ${error.message}`);
          rl.prompt();
        });

        child.on('close', (code) => {
          if (code !== 0) {
            console.error(`${command} exited with status code ${code}`);
          }
          updatePromptPlaceholders();
          rl.setPrompt(formatPrompt(customPrompt));
          rl.prompt();

          eventEmitter.emit('commandExecuted', line);
        });

        try {
          fs.writeFileSync(historyFilePath, commandHistory.join('\n'));
          } catch (error) {
            console.error(`Error saving command history: ${error.message}`);
      }
      }
    }
  }
    loadedPlugins.forEach(plugin => {
    if (typeof plugin.module.onCommand === 'function') {
      plugin.module.onCommand(line);
    }
  });
});

const expandAliases = (line) => {
  for (const alias in aliases) {
    const aliasPattern = new RegExp(`\\b${alias}\\b`, 'g');
    line = line.replace(aliasPattern, aliases[alias]);
  }
  return line;
  eventEmitter.emit('commandExecuted', line);
};


rl.on('keypress', (_, key) => {
  if (key.name === 'up' && commandIndex < commandHistory.length - 1) {
    commandIndex++;
    rl.write(null, { ctrl: true, name: 'u' });
    rl.write(commandHistory[commandHistory.length - 1 - commandIndex]);
  } else if (key.name === 'down' && commandIndex >= 0) {
    commandIndex--;
    if (commandIndex === -1) {
      rl.write(null, { ctrl: true, name: 'u' });
    } else {
      rl.write(null, { ctrl: true, name: 'u' });
      rl.write(commandHistory[commandHistory.length - 1 - commandIndex]);
    }
  }
});

rl.on('line', (line) => {
  rl.emit('export-history', commandHistory);
});

rl.on('SIGINT', () => {
  rl.question('Are you sure you want to exit? (y/n) ', (answer) => {
    if (answer.toLowerCase() === 'y') {
      rl.close();
    } else {
      rl.prompt();
    }
  });
});

rl.on('close', () => {
  console.log('Exiting custom shell. Goodbye!');
  process.exit(0);
});
