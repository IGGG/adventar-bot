function doPost(e) {
  var prop = PropertiesService.getScriptProperties().getProperties();
  
  if (prop.VERIFY_TOKEN != e.parameter.token) {
    throw new Error('invalid token.');
  }
  
  /* push empty commit */
  
  var option = { name: prop.NAME, email: prop.EMAIL };
  var github = new GitHubAPI.GitHubAPI(prop.GITHUB_USERNAME, prop.GITHUB_REPO, prop.GITHUB_TOKEN, option);
  
  var branch = github.getBranch(prop.GITHUB_BRANCH);
  var commit = github.createCommit('empty!', branch['commit']['commit']['tree']['sha'], branch['commit']['sha']);
  var result = github.updateReference(prop.GITHUB_BRANCH, commit['sha']);
  Logger.log(result);
  
  /* for Slack */
  var slackApp = SlackApp.create(prop.SLACK_API_TOKEN);

  const BOT_NAME = 'gunmer';
  const BOT_ICON = 'http://drive.google.com/uc?export=view&id=' + prop.ICON_ID;
  var option = { username : BOT_NAME, icon_url : BOT_ICON, link_names : 1 };
  var channelName = e.parameter.channel_name;

  Logger.log(slackApp.postMessage(channelName, 'just a moment...', option));
}

function test() {
  var prop = PropertiesService.getScriptProperties().getProperties();
  var e = { 
    parameter: {
      token: prop.VERIFY_TOKEN,
      text: 'update?',
      channel_name: 'bot-test'
    }
  }
  doPost(e);
}
