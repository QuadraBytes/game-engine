
# 🎮 Functional Game Engine – Haskell Backend

## Project Title
**Pure Functional Game Engine using Haskell**

## Module
**EC 8206 – Functional Programming**

---

## 👥 Group Members

- Member 1 – EG/2020/4056 - Madugalle E.W.M.W.W.N.D.B
- Member 2 – EG/2020/4078 - Morawaliyadda M.G.H.S.M 
- Member 3 – EG/2020/4228 - Thanapathi T.M.I.U.B
- Member 4 – EG/2020/4289 - Wijebandara P.A.I 

---

## 📌 Project Overview

This project implements a **pure functional game engine backend** using **Haskell**.  
It exposes a **REST API** that supports three classic games:

1. **Number Guessing Game**
2. **Tic Tac Toe**
3. **Hangman**

The system demonstrates how **functional programming principles** such as **purity, immutability, algebraic data types, and modular design** can be used to build **reliable and maintainable application logic**, especially for state-driven systems like games.

---

## 🗂️ Project Structure

```
game-engine/
├── Main.hs
├── ApiTypes.hs
├── DataTypes.hs
├── Processing.hs
├── IOHandler.hs
├── Utils.hs
├── game-engine.cabal
├── package.yaml
├── stack.yaml
└── README.md
```

---

## ⚙️ Technologies Used

- Haskell (GHC / Stack)
- Scotty (REST framework)
- Aeson (JSON serialization)
- wai-cors (CORS handling)

---

## ▶️ How to Run the Project

### Step 1: Clean
```
stack clean
```

### Step 2: Build
```
stack build
```

### Step 3: Run
```
stack run
```

Server starts at:
```
http://localhost:3001
```

---

## 🌐 API Endpoints

### Number Guessing Game
POST `/guess`

### Tic Tac Toe
POST `/ttt/move`

### Hangman
POST `/hangman/guess`

---

## 🧠 Functional Programming Concepts Used

- Pure functions
- Immutability
- Algebraic Data Types (ADTs)
- Pattern matching
- Modular design

---

## 📄 Conclusion

This project demonstrates the effectiveness of **functional programming in Haskell** for building reliable backend systems with clear state transitions and strong correctness guarantees.

## 🌐 Game UI

Open [https://game-fp-game-ui.nqmggx.easypanel.host/](https://game-fp-game-ui.nqmggx.easypanel.host/) with your browser to see the UI.
