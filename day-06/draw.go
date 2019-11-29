package main

import (
	"image"
	"image/color"
	"image/png"
	"math"
	"math/rand"
	"os"
)

func drawPartOne(input string, path string) {
	grid := parse(input)
	bounds := image.Rect(0, 0, grid.x1-grid.x0, grid.y1-grid.y0)
	img := image.NewRGBA(bounds)
	palette := color.Palette{}

	for i := 0; i < len(grid.points); i++ {
		palette = append(palette, color.RGBA{
			uint8(rand.Intn(255)),
			uint8(rand.Intn(255)),
			uint8(rand.Intn(255)),
			uint8(255),
		})
	}

	for x := 0; x < grid.x1; x++ {
		for y := 0; y < grid.y1; y++ {
			min := math.MaxInt64
			shared := false
			region := 0

			for id, b := range grid.points {
				dist := distance(x, y, b.x, b.y)

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
	for _, point := range grid.points {
		img.Set(point.x, point.y, color.White)
		img.Set(point.x-1, point.y, color.White)
		img.Set(point.x+1, point.y, color.White)
		img.Set(point.x, point.y-1, color.White)
		img.Set(point.x, point.y+1, color.White)
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

func drawPartTwo(input string, dist int, path string) {
	grid := parse(input)
	img := image.NewRGBA(image.Rect(0, 0, grid.x1-grid.x0, grid.y1-grid.y0))

	for x := 0; x < grid.x1; x++ {
		for y := 0; y < grid.y1; y++ {
			total := 0

			for _, point := range grid.points {
				total += distance(x, y, point.x, point.y)
			}

			if int(total) < dist {
				img.Set(x, y, color.Black)
			}
		}
	}

	red := color.RGBA{255, 0, 0, 255}

	// Draw a marker at each point in the grid
	for _, point := range grid.points {
		img.Set(point.x, point.y, red)
		img.Set(point.x-1, point.y, red)
		img.Set(point.x+1, point.y, red)
		img.Set(point.x, point.y-1, red)
		img.Set(point.x, point.y+1, red)
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
