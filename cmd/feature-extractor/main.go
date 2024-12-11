package main

import (
	"pluvius/internal/feature_extractor"
	"log"
)

func main() {
	err := feature_extractor.Run("../../data/raw/2019.csv")

	if err != nil {
		log.Fatal(err)
	}
}
