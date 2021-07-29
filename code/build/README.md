# Building the lectures

## Precondition
This build scripts have only been tested on OSx.

## Preparation
Copy the build scripts to the parent folder above the Plutus source code folder.
E.g. if the path to your Plutus playground server looks like that: 
`/Users/sandro/developing/learning/plutus/plutus/plutus-playground-server`

and the path to your Plutus Pioneer Program GitHub folder looks like that:
`/Users/sandro/developing/learning/plutus/plutus-pioneer-program/code/week01`

You can copy the shell scripts from this folder to
`/Users/sandro/developing/learning/plutus`

## The initial build
1. Navigate to the parent folder of the Plutus source code folder.
`/Users/sandro/developing/learning/plutus` in this example.

2. Build: Call `build.sh` with the Git commit hash corresponding to the current lecture.
   E.g. `./build.sh 8a20664f00d8f396920385947903761a9a897fe0`
   You find the hash in the `cabal.project` within the folder for the week you are working on
   There you find something like `tag: 8a20664f00d8f396920385947903761a9a897fe0`
   
   It builds the dependencies and documentation and tells you when 
   it's finished and starts the nix-shell. This way you can watch the lecture during build
   and will get notified.

3. Server: Call `cd .. && ./buildAndRunPlaygroundServer.sh`
   It builds and runs the server.

4. Client: Navigate to the Plutus source code folder, open the nix shell and run the client. 
   E.g.:
    * `cd /Users/sandro/developing/learning/plutus/plutus`
    * `nix-shell`
    * `cd .. && ./runPlaygroundClient.sh`
      Url: https://localhost:8009/

5. Docs: Navigate to the parent of the Plutus source code folder and start the documentation server. 
   E.g.:
    * `cd /Users/sandro/developing/learning/plutus`
    * `./runDocumentationServer.sh`
      Url: http://localhost:8088/

6. Repl: Navigate to the Plutus source code folder, open the nix shell 
   and run the repl with the week number (e.g.`06`) as a parameter.
   E.g.:
    * `cd /Users/sandro/developing/learning/plutus/plutus`
    * `nix-shell`
    * `cd .. && ./startRepl.sh 06`
