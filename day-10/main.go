package main

import (
	"image"
	"image/color"
	"image/gif"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type point struct {
	x int
	y int
	i int
	j int
}

var palette = color.Palette{color.Black, color.White}
var rect = image.Rect(0, 0, 600, 600)
var delay = 10

func draw(points []*point) *image.Paletted {
	img := image.NewPaletted(rect, palette)

	for _, point := range points {
		x := point.x * 2
		y := point.y * 2
		img.SetColorIndex(x+0, y+0, 1)
		img.SetColorIndex(x+1, y+0, 1)
		img.SetColorIndex(x+0, y+1, 1)
		img.SetColorIndex(x+1, y+1, 1)
	}

	return img
}

func parse(input string) []*point {
	lines := strings.Split(input, "\n")
	stars := []*point{}

	for _, line := range lines {
		r := regexp.MustCompile(`-?[0-9]+`)
		match := r.FindAllString(line, -1)
		x, _ := strconv.Atoi(match[0])
		y, _ := strconv.Atoi(match[1])
		i, _ := strconv.Atoi(match[2])
		j, _ := strconv.Atoi(match[3])
		stars = append(stars, &point{x, y, i, j})
	}

	return stars
}

func run(input string, start, end int, path string) {
	stars := parse(input)
	images := []*image.Paletted{}
	delays := []int{}

	for i := 0; i < end; i++ {
		for _, star := range stars {
			star.x += star.i
			star.y += star.j
		}

		if i >= start {
			images = append(images, draw(stars))
			delays = append(delays, delay)
		}
	}

	file, _ := os.Create(path)

	gif.EncodeAll(file, &gif.GIF{
		Image:     images,
		Delay:     delays,
		LoopCount: -1,
	})
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))

	if len(os.Args) < 3 {
		panic("Expected 2 arguments")
	}

	start, _ := strconv.Atoi(os.Args[1])
	end, _ := strconv.Atoi(os.Args[2])

	run(input, start, end, "stars.gif")
}
