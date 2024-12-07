build/feature-extractor:
	docker build . -t pluvius/feature-extractor:0.0.1 -f Dockerfile

run/feature-extractor:
	docker run -v $(file):/app/$(file) pluvius/feature-extractor:0.0.1

setup/python:
	python -m venv env
	. env/bin/activate
	pip install -r requirements.txt

download/dataset:
	. env/bin/activate
	python utils/dataset_download.py
