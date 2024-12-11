package feature_extractor

import (
	"log"
	"os"

	"github.com/gocarina/gocsv"
)

func Run(filepath string) error {
	f, file_err := get_file(filepath)

	if file_err != nil {
		log.Fatalln(file_err)
	}
	defer f.Close()

	read_chan := make(chan Weather, 1)
	go decode_file_to_channel(f, read_chan)


	// processamento
	process(read_chan)

	return nil
}

func get_file(filepath string) (*os.File, error) {
	file, file_err := os.OpenFile(filepath, os.O_RDONLY, os.ModePerm)

	if file_err != nil {
		return nil, file_err
	}
	return file, nil
}

func decode_file_to_channel(file *os.File, read_chan chan Weather) {
		unmarshal_err := gocsv.UnmarshalToChan(file, read_chan)
		if unmarshal_err != nil {
			log.Fatalln(unmarshal_err)
		}
}

func process(read_chan chan Weather) {
	for record := range read_chan {
		print(record.Date)
	}
}
