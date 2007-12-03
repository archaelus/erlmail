{application, imapd,
 [
  {description, "Erlang IMAP Server"},
  {vsn, "0.0.6"},
  {id, "imapd_listener"},
  {modules,      [imapd_listener, imapd_fsm, imapd_resp, erlmail_store]},
  {registered,   [imapd_sup, imapd_listner, imapd_resp, erlmail_store]},
  {applications, [kernel, stdlib]},
  {mod, {imapd_app, []}},
  {env, [{listen_port, 143}]}
 ]
}.
