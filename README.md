![pluvius](images/pluvius.png)

![GitHub Issues or Pull Requests](https://img.shields.io/github/issues-pr/roqueando/pluvius)

> Pluvius is a simple rain predict app for machine learning engineering study purpose


### updates and improvements

the project still in development and the next updates will be coded in the following tasks:
- [x] model training and ONNX saving
- [x] create Dockerfile for featurizer
- [x] create utils for downloading the INMET dataset
- [ ] create a worker pipeline with haskell for preprocessing
- [ ] create an evaluation API for prediction

## 💻 pre-requisites

before start, verify if you have the following things:

- you have installed the most recent version of [Haskell](https://haskell.com) and `cabal` (you can install it with [GHCup](https://www.haskell.org/ghcup/)) in your machine
- you have installed the most recent version of `python`, `pip` in your machine
- you have installed [Docker](https://docker.com) and docker compose

## 🚀 installing pluvius

For install `pluvius` first you need:

- clone repository
```sh
$ git clone https://github.com/roqueando/pluvius.git
```

- build featurizer
```sh
$ make build/featurizer
```

- create a virtualenv for python stuff
```sh
$ make setup/python
```

## ☕using pluvius

To use pluvius you will need the correct dataset, then use the download script to get the INMETBR dataset (for now I'm just using Brazilian data)
```sh
$ make download/dataset
```
This will download, extract the zip file and merge into a single CSV for post-use.

### Running preprocessing
Here we will need to pre process all dataset to fit into model, so we will run the preprocess dockerized app

```sh
$ make run/feature-extractor file=./data/raw/2019.csv
```
Make sure that you have run the `make download/dataset` step to have the merged raw file in correct directory.


## 😄 be a contributor

Want to be part of this project? Click [HERE](CONTRIBUTING.md) and read how to contribute

## 📝 license

This project is under the license. See the file [LICENSE](LICENSE.md) for more details
