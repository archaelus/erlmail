ERL=erl
APP_NAME=erlmail
NODE_NAME=erlmail
VSN=0.0.6

all:
	( $(ERL) -make && \
	if [ ! -e ebin/smptd.app ]; then cp -f src/smtpd.app.src ebin/smtpd.app; fi \
	if [ ! -e ebin/imapd.app ]; then cp -f src/imapd.app.src ebin/imapd.app; fi )

doc:	
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

run:
	$(ERL) -pa `pwd`/ebin \
	-boot start_sasl \
	-sname $(NODE_NAME)