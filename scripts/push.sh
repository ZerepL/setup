branch=$(git rev-parse --abbrev-ref HEAD)

git add -A
git commit -m "$1"
git push origin $branch
