mpeg_streamer
=============

This is work in progress.

mpeg_streamer is an application that can read mpegts files and stream them over UDP.
It has no external dependencies.

UDP flow control is done by reading PCR information from the file.

You can also analyze running UDP streams and files on disk by calling
`mpeg_streamer_server:info/1` method.

Usage
-----

Compile the package by calling

`erl -make`

Run application mpeg_streamer

```

erl -pa ebin

1> application:start(mpeg_streamer).


% Stream a file from disk via UDP multicast

2> mpeg_streamer_server:start_streamer("file:///tmp/myvideo.mpg", "udp://239.0.0.1:1234").
0

% Check what is currently being streamed

3> mpeg_streamer_server:list_streamers().
[{2,<0.1085.0>,"file:///tmp/myvideo.mpg",
  "udp://239.0.0.1:1234"}]

% Let's see what's in the files and streams

4> mpeg_streamer_server:info("file:///tmp/myvideo.mpg").
[{program,1,33,                                                   
          [{33,h264_video}]}]

5> mpeg_streamer_server:info("udp://239.0.0.1:1234").
[{program,1,33,                                                
          [{33,h264_video}]}]

% Wow, looks like we're streaming, allright.

```

Check out the file in VLC, for instance.
