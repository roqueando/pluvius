![pluvius](images/pluvius.png)

![GitHub Issues or Pull Requests](https://img.shields.io/github/issues-pr/roqueando/pluvius)

> Pluvius is a simple rain predict app for machine learning engineering studies purpose


### updates and improvements

the project still in development and the next updates will be coded in the following tasks:
- [x] model training and ONNX saving
- [] create a simple pipeline with rust and polars for serialize APIs response data for models
- [] create an evaluation API for prediction

## 💻 pre-requisites

before start, verify if you have the following things:

- you have installed the most recent version of `rust`, `cargo` in your machine
- you have installed the most recent version of `python`, `pip` in your machine
- you have any of `windows`, `macosx` or `linux` operating system (this can be compiled in any of these systems)

## 🚀 installing pluvius

For install `pluvius` from source, first you need:

- clone repository
```sh
git clone https://github.com/roqueando/pluvius.git
```

- build
```sh
cargo build
```

- change directory to `science`, install dependencies and train the model
```sh
cd science
pip install -r requirements.txt
python model.py
```
it can be a little slower depending on your PC settings

- more incoming of how to up the rust environment

## ☕using pluvius 

- still working... :D

## 😄 be a contributor 

Want to be part of this project? Click [HERE](CONTRIBUTING.md) and read how to contribute

## 📝 license

This project is under the license. See the file [LICENSE](LICENSE.md) for more details
