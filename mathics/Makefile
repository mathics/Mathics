# FIXME: We need to go over this. Much is obsolete

.PHONY: clearso restart release

put:
	rsync -av builtin core doc optional web __init__.py main.py manage.py profile.py server.py test.py tests.py urls.py --exclude "*.pyc" --exclude ".DS_Store" --exclude ".*" --exclude "*.c" --exclude "web/db" --exclude "doc/tex" jan@mathics.net:/home/jan/mathics

restart:
	ssh -t mathics.net "sudo siteadmin jan mathics restart"

release:
	make put
	make restart

clearso:
	rm -f builtin/*.so
	rm -f core/*.so

check:
	$(MAKE) -C .. $@
