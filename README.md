# calc-tweet
## installing

\# make install

## usage

you (or user) send commands to the bot via Twitter direct message. 

### set notice contents

#### input format
```
 $notice "sentence"
```

set `[連絡] "sentence"` on message head

### set date, time and venue

#### input format
```
 $date [-(num)] "date" 
 $time [-(num)] "time"
 $locale [-(num)] "locale"
```

set `"date" "time"＠"locale"` which match argument number under `[連絡] "sentence"`

### post

#### input format
```
 $post
```

post on twitter and slack

### reset 

#### input format

```
 $clear 
```
 
reset your command 

### authorize user 

#### input format

```
 $useradd "username"
```

### notify of an update about calc-web

#### input format

```
 $post-calc-web
``` 
post your good content. on twitter and slack

## example

### input
```
 $notice post test
 $date 5/15(水)
 $notice これはテストです.
 $time 10:30~11:30
 $locale 電算
 $time -2 10:40
 $locale -3 電算
 $date -2 5/16(木)
 $date 5/16(木)
 $post
```

### output
```
[連絡]
これはテストです.
5/15(木) 10:30~11:30 ＠.電算
5/16(木) 10:40
 ＠.電算
```
