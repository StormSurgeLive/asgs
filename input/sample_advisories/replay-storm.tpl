<html>
  <head>
    <title>ASGS Storm Replay Status Page</title>
  </head>
  <body>

<h1>ASGS Storm Replay Status Page</h1>

<h2>Running</h2>
[% FOREACH storm IN config.storms -%]
  [% IF config.${storm.key}.RSS_DOCROOT_URL -%]
    [% rss_root = config.${storm.key}.RSS_DOCROOT_URL -%]
  [% ELSE -%]
    [% rss_root = config.status_page.RSS_DOCROOT_URL -%]
  [% END -%]
  [% IF storm.value > 0 -%]
    <br/>[% storm.key -%] - Advisory [% tracking_ref.${storm.key}.current -%] / [% tracking_ref.${storm.key}.end -%] [<a href=[% rss_root -%]index-at.xml>rss</a>] [<a href=[% config.status_page.FTP_HOSTNAME -%][% config.status_page.FTP_URL_ROOT -%]>ftp</a>]
  [% END -%]
[% END -%]


<h2>Finished</h2>
[% FOREACH storm IN finished.storms -%]
  [% IF config.${storm.key}.RSS_DOCROOT_URL -%]
    [% rss_root = config.${storm.key}.RSS_DOCROOT_URL -%]
  [% ELSE -%]
    [% rss_root = config.status_page.RSS_DOCROOT_URL -%]
  [% END -%]
  <br/>[% storm -%] - Advisory [% tracking_ref.${stormy}.end -%] / [% tracking_ref.${storm}.end -%] [<a href=[% rss_root -%]index-at.xml>rss</a>] [<a href=[% config.status_page.FTP_HOSTNAME -%][% config.status_page.FTP_URL_ROOT -%]>ftp</a>]
[% END -%]

  <body>
</html>
