{application, smtpd,
 [
  {description, "Erlang SMTP Server"},
  {vsn, "0.0.6"},
  {id, "smtpd_listener"},
  {modules,      [smtpd_listener, smtpd_fsm]},
  {registered,   [smtpd_sup, smtpd_listner]},
  {applications, [kernel, stdlib]},
  {mod, {smtpd_app, []}},
  {env, []}
 ]
}.
