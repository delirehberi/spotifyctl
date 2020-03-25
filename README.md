# spotifyctl
This tool provides a control tool for Spotify clients. It uses Spotify Web API for interacting with a client.

## Motivation
I wanted to use the [Spotifyd](https://github.com/Spotifyd/spotifyd) tool on Nixos, but I don't want to resolve the [DBUS problem](https://community.spotify.com/t5/Desktop-Linux/Spotify-D-Bus-issue-cannot-send-CLI-commands/td-p/4528662). And I need to do practice with Haskell then I decided to write that tool.

## Feature list
- [x] Authorization to API
- [x] Get a new token when the access token is expired.
- [x] Play the current song at the active device
- [x] Pause the current song at the active device
- [x] Play the next song at the active device
- [x] Play the previous song at the active device
- [] Play the song with the given id
- [] Show the currently playing song
- [x] Get device lists
- [] Set as active the device with the given id
- [] Set as active the first device in the device list
- [] Refresh the access token automatically

## Usage
I implement play, pause, next,  previous commands yet. The tool work in progress. But still, you may want to use this tool; you have to build yourself with Stack. Before the building, you have to get a client id and client secret from spotify.Set callback url as `http://127.0.0.1:9988/oauthCallback`

Export the environment variables. And then, you can build the application.

```
export CLIENTID=clientid
export CLIENTSECRET=clientsecret
```

And then, run the spotifyctl auth command and give permission.

For first run, you need set a client as active in spotify web player.

## Commands
- [x] auth
- [x] refresh
- [x] play
- [x] pause
- [x] next
- [x] prev
- [] play SONGID
- [] current
- [x] device list
- [] device select
- [] device select DEVICEID


Your contributions are welcome. Feel free to open an issue when you find a bug.