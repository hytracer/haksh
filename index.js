const readline = require('readline');
const { spawn } = require('child_process');
const fs = require('fs');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: 'haksh> ',
  historySize: 100,
});

rl.prompt();

const commandHistory = [];
let commandIndex = -1;

rl.on('line', (line) => {
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
        } catch (error) {
          console.error(`cd: ${error.message}`);
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
          rl.prompt();
        });
      }
    }
  }
});

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
