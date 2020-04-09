# spotifyctl
This tool provides a control tool for Spotify clients. It uses Spotify Web API for interacting with a client.

## Motivation
I wanted to use the [Spotifyd](https://github.com/Spotifyd/spotifyd) tool on Nixos, but I don't want to resolve the [DBUS problem](https://community.spotify.com/t5/Desktop-Linux/Spotify-D-Bus-issue-cannot-send-CLI-commands/td-p/4528662). And I need to do practice with Haskell then I decided to write that tool.

## Feature list
- [x] Setup
- [x] Authorization to API
- [x] Get a new token when the access token is expired.
- [x] Play the current song at the active device
- [x] Pause the current song at the active device
- [x] Play the next song at the active device
- [x] Play the previous song at the active device
- [ ] Play the song with the given id
- [ ] Show the currently playing song
- [x] Get device lists
- [x] Set as active the device with the given id
- [x] Set as active the first device in the device list
- [ ] Refresh the access token automatically

## Usage
I implement play, pause, next,  previous and device commands yet. The tool work in progress. But still, you may want to use this tool; you have to build yourself with Stack. Before the building, you have to get a client id and client secret from spotify.Set callback url as `http://127.0.0.1:9988/oauthCallback`

And then, run the `spotifyctl setup CLIENT_ID CLIENT_SECRET` command and follow the redirections. Do not forget replace your CLIENT_ID and CLIENT_SECRET values to command.

You need an active client for using this tool. I use spotifyd. You can select the active device with `spotifyctl device select` command. You can select a specified device with `spotifyctl device select DEVICENAME` command. You can view active device list with `spotifyctl device list` command.

## Commands
- [x] setup
- [x] auth
- [x] refresh
- [x] play
- [x] pause
- [x] next
- [x] prev
- [ ] play SONGID
- [ ] current
- [x] device list
- [x] device select
- [x] device select DEVICEID


Your contributions are welcome. Feel free to open an issue when you find a bug.