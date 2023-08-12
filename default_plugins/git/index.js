const fs = require('fs');
const path = require('path');

// Function to get the current Git branch
function getGitBranch() {
  try {
    const gitHeadPath = path.join(process.cwd(), '.git', 'HEAD');
    const ref = fs.readFileSync(gitHeadPath, 'utf8').trim();
    if (ref.startsWith('ref:')) {
      return ref.split('/').pop();
    }
    return 'Detached';
  } catch (error) {
    return 'Unknown';
  }
}

// Function to get Git status
function getGitStatus() {
  // Replace this with actual code to get Git status
  // For simplicity, let's assume the status is "Clean" or "Dirty"
  return 'Clean';
}

// Git plugin
module.exports = {
  // Function to add Git-related placeholders
  applyToPlaceholders: (placeholders) => {
    placeholders['%g'] = getGitBranch();
    placeholders['%s'] = getGitStatus();
  },

  applyToPlaceholdersOnUpdate: (placeholders) => {
    return () => {
      placeholders['%g'] = getGitBranch();
      placeholders['%s'] = getGitStatus();
    };
  },

  // Event listener for executed commands
  onCommandExecuted: (command) => {
      // console.log("updating git")
  }
};
