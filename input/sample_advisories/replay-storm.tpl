<html>
  <head>
    <title>ASGS Storm Replay Status Page</title>
  </head>
  <body>

<h1>ASGS Storm Replay Status Page</h1>

<h2>Running</h2>
[% FOREACH storm IN config.storms -%]
  <br/>[% storm.key -%] - Advisory [% tracking_ref.${storm.key}.current -%] / [% tracking_ref.${storm.key}.end -%] [<a href=/index-at.xml>rss</a>] [<a href=[% config.status_page.FTP_URL -%]>ftp</a>]
[% END -%]

<h2>Finished</h2>
[% FOREACH storm IN finished.storms -%]
  <br/>[% storm -%] - Advisory [% tracking_ref.${stormy}.end -%] / [% tracking_ref.${storm}.end -%] [<a href=/index-at.xml>rss</a>] [<a href=[% config.status_page.FTP_URL -%]>ftp</a>]
[% END -%]

  <body>
</html>
