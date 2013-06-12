
if [ $# -lt 1 ]; then
	echo "Usage: $0 remote"
	exit 1
fi

git remote show $1 > /dev/null 2> /dev/null

if [ $? -ne 0 ]; then
	echo "unknown remote: $1"
	echo "Valid remotes are..."
	REMOTES=`git remote`
	for r in $REMOTES
	do
		echo " $r"
	done
	exit 1
fi

cat << EOF

Hi `whoami`,

I will help you clean up some GIT branches.  I'm going to walk
you through the branches on the remote $1.  You'll have to
press the 'y' key to add branches to the delete list.

Once all the branches have been iterated, I'll let you review
the choices you made.  Then, if you tell me to, I'll output
the GIT commands to delete those branches from $1.
You'll need to enter these commands yourself to do the actual
work.

If you haven't run...

$ git remote prune $1

...lately you should Ctrl+C and do it now.  Otherwise, you may
try to delete branches that have already been deleted by somebody
else.

Also, please don't delete the 'master' or any of the 'rel'
branches.  Those branches are important!

Thanks in advnace,
$0

EOF

read -p "Press any key to continue..." x

BRANCHES=`git branch -r | grep $1 | grep -v $1/HEAD | xargs`
COUNT=8

DELETE_LIST=""

for b in $BRANCHES
do
	echo ""
	echo "$b, last commit was `git log --pretty=format:'%cd' -n 1 --date=relative $b` by <`git log --pretty=format:'%an' -n 1 $b`>"
	git log --pretty=format:'* %h - %s (%cr) <%an>' --abbrev-commit --date=relative -n $COUNT $b | cat
	echo ""
	echo ""
	read -p "Delete the branch $b (y,n) [n]? " yn
	case $yn in
		[Yy]* ) DELETE_LIST="$DELETE_LIST $b";;
	esac
	echo ""
done



if [ "$DELETE_LIST" == "" ]; then
	echo "Nothing selected to delete."
	exit 0
fi



for b in $DELETE_LIST
do
	echo -n "$b <`git log --pretty=format:'%an' -n 1 $b`>"
	if [ "$b" == "$1/master" ] ; then
		echo "!!! Warning! You\'re about to do something epicly stupid !!!"
	else
		echo
	fi
done



while true; do
echo ""
read -p "Output commands to delete the above branches? (Yes,No)? " yn
case $yn in
	Yes)
		echo "-----Copy and Paste these GIT commands to clean up-----"
		for b in $DELETE_LIST
		do
			branch=${b//$1\//}
			echo "git push $1 :$branch"
		done
		echo "-------------------------------------------------------"
		exit;;
	No|no) 
		exit;;
	*)  echo "You didn't type out \"Yes\" or \"No\"";;
esac
done

