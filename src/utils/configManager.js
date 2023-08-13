// web-server.js
const express = require('express');
const bodyParser = require('body-parser');
const fs = require('fs');
const path = require('path');

const ansiStyles = import('ansi-styles');

class WebServer {
  constructor(configPath) {
    this.configPath = configPath;
    this.app = express();
    this.app.use(bodyParser.urlencoded({ extended: true }));
    this.app.use(express.static(path.join(__dirname, 'public')));
    this.app.get('/', this.handleEditorRequest.bind(this));
    this.app.post('/save', this.handleSaveRequest.bind(this));
  }

  start(port) {
    this.server = this.app.listen(port, () => {
      console.log(`Config editor started on port ${port}!\nBrowse http://localhost:${port} to edit your configuration files.\nTo stop the web server, run hakshconfig quit`);
    });
  }

  stop() {
    this.server.close();
    console.log('haksh config server stopped.');
  }

handleEditorRequest(req, res) {
  const hakshrcContent = fs.readFileSync(this.configPath, 'utf-8');
  const html = `
    <!DOCTYPE html>
    <html>
    <head>
      <title>Haksh Configuration Editor</title>
<script>
  function updatePromptPreview() {
    const input = document.getElementById('config-input');
    const preview = document.getElementById('prompt-preview');
    const lines = input.value.split('\\n'); // Split by newlines
    let ps1Line = '';

    for (const line of lines) {
      if (line.trim().startsWith('PS1=')) {
        ps1Line = line;
        break;
      }
    }

    const value = ps1Line.replace(/\\x1b\\[([0-9]{1,2}(;[0-9]{1,2})*)?m/g, '');

    preview.innerHTML = value;
  }
</script>
    </head>
    <body>
      <h1>Haksh Configuration Editor</h1>
      <form action="/save" method="post">
        <textarea id="config-input" name="config" rows="10" cols="50" onkeyup="updatePromptPreview()">${hakshrcContent}</textarea>
        <br>
        <button type="submit">Save</button>
      </form>
      <div>
        <h2>Prompt Preview:</h2>
        <pre id="prompt-preview">... start editing to preview your prompt live</pre>
      </div>
    </body>
    </html>
  `;
  res.send(html);
}


  handleSaveRequest(req, res) {
    const newConfig = req.body.config;
    fs.writeFileSync(this.configPath, newConfig, 'utf-8');
    res.redirect('/');
  }
}

module.exports = WebServer;
