build/featurizer:
	docker build . -t pluvius/featurizer:0.0.1 -f featurizer/Dockerfile

run/featurizer:
	docker run pluvius/featurizer:0.0.1

setup/python:
	python -m venv env
	. env/bin/activate
	pip install -r requirements.txt

download/dataset:
	. env/bin/activate
	python utils/dataset_download.py
