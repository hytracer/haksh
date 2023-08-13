// web-server.js
const express = require('express');
const bodyParser = require('body-parser');
const fs = require('fs');
const path = require('path');

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
      </head>
      <body>
        <h1>Haksh Configuration Editor</h1>
        <form action="/save" method="post">
          <textarea name="config" rows="10" cols="50">${hakshrcContent}</textarea>
          <br>
          <button type="submit">Save</button>
        </form>
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
