
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



BRANCHES=`git branch -r | grep $1 | grep -v $/HEAD | xargs`
COUNT=8

for b in $BRANCHES
do
	echo ""
	echo "$b, last commit was `git log --pretty=format:'%cd' -n 1 --date=relative $b` by <`git log --pretty=format:'%an' -n 1 $b`>"
	git log --pretty=format:'* %h - %s (%cr) <%an>' --abbrev-commit --date=relative -n $COUNT $b | cat
	echo ""
	echo ""
done

