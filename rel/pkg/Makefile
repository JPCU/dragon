VERSION=0.7.0
COMPONENT_INTERNAL=dragon
COMPONENT=dragon

TARGET_DIR ?=/opt/local
FILE ?=$(COMPONENT)-$(VERSION)$(SUFFIX)
BLOCK_SIZE ?=65536
STAGE_DIR ?=deploy
TMP_DIR ?=tmp
PKG_CATEGORY ?=fifo
PKG_HOMEPAGE ?=https://project-fifo.net


DEPS="erlang" "coreutils" "sudo" "grep"

.PHONY: prepare package_list build_info clean-pkg

# prepare:
# 	echo $(PWD)
# 	-rm -r ./$(COMPONENT) || true
# 	mkdir -p ./$(COMPONENT)
# 	#ls -R ../../_build/prod/rel/$(COMPONENT_INTERNAL)
# 	cp -r ../../_build/prod/rel/$(COMPONENT_INTERNAL) ./

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT) || true
	mkdir -p $(STAGE_DIR)/$(COMPONENT)
	cp -r ../../_build/prod/rel/$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)


package: prepare $(FILE).tgz


clean:
	-rm *.tgz build-info packlist

package_list:
	-rm packlist || true
	for dep in $(DEPS); do echo "@pkgdep $$dep" >> packlist; done
	(cd $(STAGE_DIR); find * -type f | sort) >> packlist

build_info:
	pkg_info -X pkg_install | egrep '^(MACHINE_ARCH|OPSYS|OS_VERSION|PKGTOOLS_VERSION)' >build-info
	echo "CATEGORIES=$(PKG_CATEGORY)" >>build-info
	echo "HOMEPAGE=$(PKG_HOMEPAGE)" >>build-info

clean-pkg:
	-rm -r tmp build-info packlist


tmp/$(FILE).tgz: package_list build_info
	-rm -r tmp || true
	mkdir tmp
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I $(TARGET_DIR) -p $(STAGE_DIR) -U tmp/$(FILE).tgz



$(FILE).tgz: tmp/$(FILE).tgz
	cp tmp/$(FILE).tgz $(FILE).tgz
	rm -r tmp