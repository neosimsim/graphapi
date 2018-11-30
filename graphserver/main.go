package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"flag"
	"path"
)

var repo *FileRepo

func main() {
	httpListen := flag.String("http", ":8080", "address for incoming connections")
	dir := flag.String("dir",  ".", "directory to store to and read from")
	flag.Parse()

	repo = NewFileRepo(path.Join(*dir, "elements"))
	http.HandleFunc("/elements/", ServeFileFactory(repo))
	linkRepo := NewFileRepo(path.Join(*dir, "links"))
	http.HandleFunc("/links/", ServeFileFactory(linkRepo))
	http.Handle("/assets/", http.StripPrefix("/assets", NewCorsHandler(http.FileServer(http.Dir("assets")))))

	log.Fatal(http.ListenAndServe(*httpListen, nil))
}

func ServeFileFactory(repo Repo) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		switch req.Method {
		case "GET":
			log.Print("GET")
			id := path.Base(req.URL.Path)
			if id == "elements" {
				ReadElements(repo, w, req)
			} else {
				ReadElement(repo, w, req)
			}
		case "POST":
			log.Print("POST")
			CreateElements(repo, w, req)
		case "PUT":
			log.Print("PUT")
			UpdateElements(repo, w, req)
		case "DELETE":
			log.Print("DELETE")
			DeleteElements(repo, w, req)
		}
	}
}

type CorsHandler struct {
	handler http.Handler
}

func NewCorsHandler(handler http.Handler) *CorsHandler {
	return &CorsHandler{handler: handler}

}

func (ch *CorsHandler) ServeHTTP(rw http.ResponseWriter, req *http.Request) {
	rw.Header().Set("Access-Control-Allow-Origin", "http://localhost:3000")
	rw.Header().Set("Access-Control-Allow-Methods", "GET")
	ch.handler.ServeHTTP(rw, req)
	rw.Header().Set("Access-Control-Allow-Origin", "http://localhost:3000")
}

func genUUID() string {
	f, _ := os.Open("/dev/urandom")
	b := make([]byte, 16)
	f.Read(b)
	f.Close()
	return fmt.Sprintf("%x-%x-%x-%x-%x", b[0:4], b[4:6], b[6:8], b[8:10], b[10:])
}
