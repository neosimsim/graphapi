package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"path"

	graphjson "github.com/neosimsim/graphapi/json"
)

func main() {
	httpListen := flag.String("http", ":8080", "address for incoming connections")
	dir := flag.String("dir", ".", "directory to store to and read from")
	flag.Parse()

	elementRepo := NewFileRepo(path.Join(*dir, "elements"))
	http.HandleFunc("/elements/", ServeFileFactory("/elements/", elementRepo))
	linkRepo := NewFileRepo(path.Join(*dir, "links"))
	http.HandleFunc("/links/", ServeFileFactory("/links/", linkRepo))
	http.HandleFunc("/typeinfo/", TypeInfoFactory(*dir))
	http.Handle("/assets/", http.StripPrefix("/assets", NewCorsHandler(http.FileServer(http.Dir("assets")))))

	log.Fatal(http.ListenAndServe(*httpListen, nil))
}

func ServeFileFactory(urlPath string, repo Repo) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		switch req.Method {
		case "GET":
			log.Print("GET")
			if req.URL.Path == urlPath {
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

func TypeInfoFactory(basedir string) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		typeName := path.Base(req.URL.Path)
		if typeName == "typeinfo" {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte("type name required"))
		} else {
			if f, err := os.Open(path.Join(basedir, "schemas", typeName)); err != nil {
				w.WriteHeader(http.StatusBadRequest)
				w.Write([]byte(err.Error()))
			} else if info, err := graphjson.ReadPropertyInfo(f); err != nil {
				w.WriteHeader(http.StatusInternalServerError)
				w.Write([]byte(err.Error()))
			} else {
				jsonEncoder := json.NewEncoder(w)
				if err := jsonEncoder.Encode(info); err != nil {
					w.WriteHeader(http.StatusInternalServerError)
					w.Write([]byte(err.Error()))
				}
			}
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
