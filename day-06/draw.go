package main

import (
	"image"
	"image/color"
	"image/png"
	"math"
	"math/rand"
	"os"
)

func DrawPartOne(input string, path string) {
	grid := ParseGrid(input)
	img := image.NewRGBA(image.Rect(0, 0, grid.X1-grid.X0, grid.Y1-grid.Y0))
	palette := color.Palette{}

	for i := 0; i < len(grid.Points); i++ {
		palette = append(palette, color.RGBA{
			uint8(rand.Intn(255)),
			uint8(rand.Intn(255)),
			uint8(rand.Intn(255)),
			uint8(255),
		})
	}

	for x := 0; x < grid.X1; x++ {
		for y := 0; y < grid.Y1; y++ {
			min := math.MaxInt64
			shared := false
			region := 0

			for id, b := range grid.Points {
				dist := Distance(x, y, b.X, b.Y)

				if dist <= min {
					region = id
					shared = dist == min
					min = dist
				}
			}

			if shared == false {
				img.Set(x, y, palette[region])
			}
		}
	}

	// Draw a cross at each point in the grid
	for _, point := range grid.Points {
		img.Set(point.X, point.Y, color.White)
		img.Set(point.X-1, point.Y, color.White)
		img.Set(point.X+1, point.Y, color.White)
		img.Set(point.X, point.Y-1, color.White)
		img.Set(point.X, point.Y+1, color.White)
	}

	file, err := os.Create(path)

	if err != nil {
		panic(err)
	}

	err = png.Encode(file, img)

	if err != nil {
		panic(err)
	}
}

func DrawPartTwo(input string, distance int, path string) {
	grid := ParseGrid(input)
	img := image.NewRGBA(image.Rect(0, 0, grid.X1-grid.X0, grid.Y1-grid.Y0))

	for x := 0; x < grid.X1; x++ {
		for y := 0; y < grid.Y1; y++ {
			total := 0

			for _, point := range grid.Points {
				total += Distance(x, y, point.X, point.Y)
			}

			if int(total) < distance {
				img.Set(x, y, color.Black)
			}
		}
	}

	file, err := os.Create(path)

	if err != nil {
		panic(err)
	}

	err = png.Encode(file, img)

	if err != nil {
		panic(err)
	}
}
