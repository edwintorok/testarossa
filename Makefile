HOSTS=xcluster1 xcluster2 xcluster3 xcluster4 xcluster5 xcluster6 xcluster7 xcluster8 xcluster9 xcluster10 xcluster11 xcluster12 xcluster13 xcluster14 xcluster15 xcluster16
ssh-config: $(foreach host,$(HOSTS),.vagrant/machines/$(host)/xenserver/id)
	vagrant ssh-config $(HOSTS) > $@

.PHONY: test clean watch
test: ssh-config
	nosetests --verbosity=3 tests/cluster_demo.py

watch: ssh-config
	scripts/tmuxmulti.sh 'while ! ssh -t -F ssh-config {} sudo -E  corosync-quorumtool -m; do sleep 1; done' $(HOSTS)
#	scripts/tmuxmulti.sh 'while ! ssh -t -F ssh-config {} sudo -E tail -f /var/log/daemon.log; do sleep 1; done' $(HOSTS)

clean:
	rm -f ssh-config
