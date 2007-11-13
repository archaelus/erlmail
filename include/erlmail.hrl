-ifndef(D).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.
-ifndef(CRLF).
-define(CRLF,[13,10]).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.

-define(ERLMAIL_VERSION,"0.0.6").

-record(store,{
	name = [],
	options = []
	}).

-record(domain,{
	name    = [], % Domain Name
	options = []  % Key/Value list of options
	}).

-record(user,{
	name       = [], % Tuple {Users Name, Domain Name}
	password   = [], % User Password - check options for HASH type, otherwise plain text
	options    = []  % key/Value list of options
	}).

-record(message,{
	name       = [], % Tuple {Message Name, User Name, Doamin Name}
	options    = [], % Key/Value list of options
	folder     = [], % FolderName; default is blank for inbox
	uid        = 0,  % Unique Identifier
	flags      = [], % IMAP flags in proplist
	message    = []  % Whole Mail Message
	}).

-record(mailbox_store,{
	name       = [], % Tuple {MailBoxName,USerName,DomainName}
	subscribed = false,
	uidnext     = 1,
	uidvalidity = 0,
	options    = [],
	messages   = []
	}).