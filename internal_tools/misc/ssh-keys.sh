#!/bin/bash

if [ "x$1" == "x" ] ; then
	echo "Usage: $0 userid"
	echo ""
	echo "what's your user name?"
	exit 1
fi

NAME=$1

if [ ! -f ~/.ssh/id_rsa.pub ] ; then
	ssh-keygen -N "" -t rsa -f ~/.ssh/id_rsa
fi

cat ~/.ssh/id_rsa.pub | \
ssh -q "$NAME@hoops3d.com" "if [ ! -d ~/.ssh/ ] ; then mkdir ~/.ssh ; fi ; chmod 700 ~/.ssh/ ; cat - >> ~/.ssh/authorized_keys ; sort -u ~/.ssh/authorized_keys > ~/.ssh/authorized_keys_sorted ; mv ~/.ssh/authorized_keys_sorted ~/.ssh/authorized_keys ; chmod 600 ~/.ssh/authorized_keys"

