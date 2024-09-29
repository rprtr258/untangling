FROM ghcr.io/rprtr258/serve-git:fc698f1c88476cd0b1e182ac9e833211377f40ef
CMD ["--repo", "https://github.com/rprtr258/untangling", "--branch", "master", "--base", "/"]
