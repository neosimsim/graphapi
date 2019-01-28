package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"path"
	"time"

	"github.com/fsnotify/fsnotify"
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
	http.HandleFunc("/tree/", TreeSearchFactory(*dir, "/tree/"))
	http.Handle("/assets/", http.StripPrefix("/assets", NewCorsHandler(http.FileServer(http.Dir("assets")))))

	log.Fatal(http.ListenAndServe(*httpListen, nil))
}

func ServeFileFactory(urlPath string, repo Repo) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		switch req.Method {
		case http.MethodGet:
			log.Print(req.Method)
			if req.URL.Path == urlPath {
				ReadElements(repo, w, req)
			} else {
				ReadElement(repo, w, req)
			}
		case http.MethodPost:
			log.Print(req.Method)
			CreateElements(repo, w, req)
		case http.MethodPut:
			log.Print(req.Method)
			UpdateElements(repo, w, req)
		case http.MethodDelete:
			log.Print(req.Method)
			DeleteElements(repo, w, req)
		}
	}
}

func TypeInfoFactory(basedir string) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		if req.Method != http.MethodGet {
			w.WriteHeader(http.StatusMethodNotAllowed)
			return
		}
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

func TreeSearchFactory(basedir, urlPath string) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		if req.Method != http.MethodPost {
			w.WriteHeader(http.StatusMethodNotAllowed)
			return
		}
		searchDirPath := path.Join(basedir, "queries", genUUID())
		os.MkdirAll(searchDirPath, os.ModeDir|os.ModePerm)
		queryPath := path.Join(searchDirPath, "query")
		if queryFile, err := os.Create(queryPath); err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			w.Write([]byte(err.Error()))
			return
		} else {
			watcher, err := fsnotify.NewWatcher()
			if err != nil {
				w.WriteHeader(http.StatusInternalServerError)
				w.Write([]byte(err.Error()))
				return
			}
			defer watcher.Close()

			result := make(chan string)
			go func() {
				for {
					select {
					case event, ok := <-watcher.Events:
						log.Println("fs event:", event)
						if !ok {
							return
						}
						if event.Op == fsnotify.Write && path.Base(event.Name) == "result" {
							log.Println("found result")
							time.Sleep(1 * time.Millisecond) // ugly workaround: Some times the file is still empy a few moments after the event has been sent.
							result <- event.Name
							return
						}
					case err, ok := <-watcher.Errors:
						if !ok {
							return
						}
						log.Println("error:", err)
					}
				}
			}()

			err = watcher.Add(searchDirPath)
			if err != nil {
				log.Print(err)
				w.WriteHeader(http.StatusInternalServerError)
				w.Write([]byte(err.Error()))
				return
			}

			io.Copy(queryFile, req.Body)
			resultPath := <-result
			if resultFile, err := os.Open(resultPath); err != nil {
				log.Print(err)
				w.WriteHeader(http.StatusInternalServerError)
				w.Write([]byte(err.Error()))
			} else {
				defer func() {
					resultFile.Close()
				}()
				log.Printf("reading result from %s", resultPath)
				w.WriteHeader(http.StatusOK)
				io.Copy(w, resultFile)
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
