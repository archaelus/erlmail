{application, erlmail,
 [
  {description, "Erlang Email Server"},
  {vsn, "0.0.6"},
  {id, "erlmail"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {erlmail_app, []}},
  {env, [
  	{server_smtp_start,false},
	{server_smtp_name,"smtp.erlsoft.net"},
  	{server_smtp_port,25},
	{server_smtp_max_connection,25},
	{server_smtp_greeting,"ErlMail http://erlsoft.org (NO UCE)"},

	{server_imap_start,false},
	{server_imap_name, "imap.erlsoft.net"},
	{server_imap_greeting, "ErlMail IMAP4 server ready"},
	{server_imap_greeting_capability, false},
	{server_imap_port,143},
	{server_imap_hierarchy, "/"},
	{server_imap_extentions,[]},

	{server_pop_start, false},
	{server_pop_port,110},

	{store_type_domain, mnesia_store},
	{store_type_user, mnesia_store},
	{store_type_message, mnesia_store},
	{store_type_mailbox_store, mnesia_store},

	{mnesia_table_domain, erlmail_domain},
	{mnesia_table_user, erlmail_user},
	{mnesia_table_message, erlmail_message},
	{mnesia_table_mailbox_store, erlmail_mailbox_store}
	]}
 ]
}.








