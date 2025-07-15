package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
)

const (
	TargetURL = "https://www.researchcatalogue.net/portal/search-result?fulltext=&title=&autocomplete=&keyword=%s&portal=%s&statusprogress=0&statuspublished=0&includelimited=0&includeprivate=0&type_research=research&resulttype=research&modifiedafter=&modifiedbefore=&format=json&limit=250&page=0"
	Port      = ":3000"
)

func handler(w http.ResponseWriter, r *http.Request) {
	keyword := r.URL.Query().Get("keyword")
	portal := r.URL.Query().Get("portal")

	if portal == "" {
		portal = ""
	}

	targetURL := fmt.Sprintf(TargetURL, url.QueryEscape(keyword), url.QueryEscape(portal))
	resp, err := http.Get(targetURL)
	if err != nil {
		http.Error(w, "Error fetching data", http.StatusInternalServerError)
		log.Println("Error fetching data:", err)
		return
	}
	defer resp.Body.Close()

	_, err = io.Copy(w, resp.Body)
	if err != nil {
		http.Error(w, "Error writing response", http.StatusInternalServerError)
		log.Println("Error writing response:", err)
	}
}

func main() {
	http.HandleFunc("/proxy", handler)

	log.Printf("Proxy server running on http://localhost%s", Port)
	if err := http.ListenAndServe(Port, nil); err != nil {
		log.Fatal("ListenAndServe: ", err)
	}
}