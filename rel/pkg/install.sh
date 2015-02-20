#!/usr/bin/bash

USER=dragon
GROUP=$USER

case $2 in
    PRE-INSTALL)
        #if grep '^Image: base64 1[34].[1234].*$' /etc/product
        #then
        #    echo "Image version supported"
        #else
        #    echo "This image version is not supported please use the base64 13.2.1 image."
        #    exit 1
        #fi
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating sniffle group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating sniffle user ...
            useradd -g $GROUP -s /bin/false $USER
        fi
        echo Creating directories ...
        mkdir -p /var/log/dragon
        chown -R dragon:dragon /var/log/dragon
        mkdir -p /var/db/dragon
        chown -R dragon:dragon /var/db/dragon
        mkdir -p /var/log/dragon/sasl
        chown -R dragon:dragon /var/log/dragon


        if [ -d /tmp/dragon ]
        then
            chown -R dragon:dragon /tmp/dragon/
        fi
        ;;
    POST-INSTALL)
        # TODO!
        echo Importing service ...
        svccfg import /opt/local/dragon/share/dragon.xml
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | awk -e '{print $2}'`
        CONFFILE=/opt/local/dragon/etc/dragon.conf
        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
            sed --in-place -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
        else
            echo "It appears that there is already a config file on this system. Please check for new configuration options."
        fi
        ;;
esac