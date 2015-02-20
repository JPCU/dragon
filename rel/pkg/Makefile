VERSION=0.6.1
COMPONENT_INTERNAL=dragon
COMPONENT=dragon

.PHONY: build_info package package_list

package: package_list build_info
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I /opt/local -p deploy -U $(COMPONENT)-$(VERSION)$(SUFFIX).tgz

package_list:
	echo "@pkgdep erlang>=16.1.1" > packlist
	#- rm -r deploy/$(COMPONENT)
	cp -r ../$(COMPONENT_INTERNAL) deploy/$(COMPONENT)
	(cd deploy/; find * -type f | sort) >> packlist

build_info:
	pkg_info -X pkg_install | egrep '^(MACHINE_ARCH|OPSYS|OS_VERSION|PKGTOOLS_VERSION)' >build-info
	echo "CATEGORIES=security" >>build-info
	echo "HOMEPAGE=http://github.com/JPCU/dragon" >>build-info

clean:
	-rm *.tgz build-info packlist