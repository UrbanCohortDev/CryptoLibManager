# CryptoLibManager
Help with installing the CryptoLibs from Xor-el

This  application enables you to easily install the crypto libraries developed by Xor-el. 
These can be found at https://github.com/Xor-el. This application will clone and update the required repositories direct from GitHub. You will need Git installed on your PC.

One issue is that they are multi level directory wise and that can make it hard work to install into multiple Delphi Versions. This app will do that for you, if that's the scheme you want.

The only 3rd party package required is TurboPack/DOSCommand. This is avalailable from GetIt or direct from Github https://github.com/TurboPack/DOSCommand.


The app gives you 3 options:
- create a single folder with all the libraries combined, but not mapped into Delphi's library paths. This is what to use if you plan to add the search path to your project's environment as you only need to one path; 
- create a single folder with all the libraries combined and map that path into Delphi's library paths. 
- Map all the required folders to the Delphi Library paths

Tidy it up and send a pull request if you fancy it.

