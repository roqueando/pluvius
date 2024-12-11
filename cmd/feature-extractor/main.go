package main

import (
	"log"
	"pluvius/internal/feature_extractor"
)

func main() {
	err := feature_extractor.Run("./data/raw/2019.csv")

	if err != nil {
		log.Fatal(err)
	}
}
