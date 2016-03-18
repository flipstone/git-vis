# git-vis

An instructional visualizer for Git databases

*This tool was built for instructional purposes. It will load the entirety of the git database you
point it at. If you attempt to run it on a project of any real size it will likely crash and
burn horribly. You have been warned.*

## To build and run from source

    docker-compose run --rm --service-ports dev bash
    cabal update
    cabal sandbox init
    cabal install
    ./.cabal-sandbox/bin/git-vis <path to a .git directory>
    

Then point your browser to `http://<your docker host ip>:8023/`

