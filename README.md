
# Card Game DSL

This project allows you to define card games using a custom Domain-Specific Language (DSL), parse the DSL into JSON format using Haskell, and execute the game logic in Python.

## Requirements

### Tools
- **Haskell**: GHC (Haskell Compiler) and Cabal (Haskell Package Manager)
- **Python**: Python 3.x (Any modern version)

### Haskell Libraries
- `aeson`: For JSON serialization
- `parsec`: For parsing the DSL
- `bytestring`: For handling byte strings

---

## Setup Guide

### 1. Install Required Tools

#### Haskell Installation
1. Install Haskell via [GHCup](https://www.haskell.org/ghcup/).
2. Verify the installation:
   ```bash
   ghc --version
   cabal --version
   ```


### 2. Install Haskell Libraries

Run the following commands to install the required libraries:

```bash
cabal install aeson --lib
cabal install parsec --lib
cabal install bytestring --lib
```

To verify the libraries are installed:

```bash
ghc-pkg list
```

---

### 3. Set Up the DSL File

Create a file named `game.txt` 


### 4. Compile the Haskell Program

Ensure you have two Haskell files:
1. `CardGameParser.hs`: Contains the parser logic.
2. `Main.hs`: Contains the entry point to parse the DSL.

Compile the Haskell program:

```bash
ghc -o CardGameParser -package parsec -package aeson Main.hs CardGameParser.hs
```

---

### 5. Run the Haskell Program

Run the compiled executable to parse `game.txt` and generate `game.json`:

```bash
./CardGameParser
```

After this step, you should see a `game.json` file in the same directory.

---

### 6. Run the Python Program

Run the Python script:

```bash
python card_game.py
```

---


## Directory Structure

Ensure the following structure for the project:

```
/CardGameDSL
   |- Main.hs
   |- CardGameParser.hs
   |- game.txt
   |- card_game.py
   |- game.json (generated after running the Haskell program)
```

---

Feel free to reach out if you encounter issues!
