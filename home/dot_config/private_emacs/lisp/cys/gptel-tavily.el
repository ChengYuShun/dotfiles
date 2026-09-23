;; -*- lexical-binding: t -*-

;; This file is mostly copied from:
;; https://github.com/ksqsf/emacs-config/blob/master/modules/prelude-ai.el
;; with some modifications made.

(defun tavily-search-async (callback query &optional search-depth max-results)
  "Perform a search using the Tavily API and return results as JSON string.
API-KEY is your Tavily API key.
QUERY is the search query string.
Optional SEARCH-DEPTH is either \"basic\" (default) or \"advanced\".
Optional MAX-RESULTS is the maximum number of results (default 5)."
  (let* ((url "https://api.tavily.com/search")
         (search-depth (or search-depth "basic"))
         (max-results (or max-results 5))
         (request-data
          `(("api_key" . ,tavily-api-key)
            ("query" . ,query)
            ("search_depth" . ,search-depth)
            ("max_results" . ,max-results))))
    (plz 'post url
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-data)
      :as 'string
      :then (lambda (result) (funcall callback result)))))

(defun tavily-extract-async (callback urls &optional format)
  (let* ((request-url "https://api.tavily.com/extract")
         (format (if (equal format "text")
                     "text"
                   "markdown"))
         (request-data
          `(("api_key" . ,tavily-api-key)
            ("urls" . ,urls)
            ("format" . ,format))))
    (plz 'post request-url
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-data)
      :as 'string
      :then (lambda (result) (funcall callback result)))))

(defun fetch-url-text-async (callback url)
  "Fetch text content from URL."
  (interactive "sEnter URL: ")
  (require 'shr)
  (plz 'get url
       :as 'string
       :connect-timeout 5
       :timeout 10
       :then (lambda (html)
               (with-temp-buffer
                 (insert html)
                 (shr-render-region (point-min) (point-max))
                 (funcall callback (buffer-substring-no-properties
                                    (point-min) (point-max)))))
       :else (lambda (err) (funcall callback "Error"))))

(gptel-make-tool
 :category "web"
 :name "search"
 :async t
 :function #'tavily-search-async
 :description "Search the internet; if you use any search results, be sure to include the references in your response."
 :args '((:name "keyword" :type string :description "The keyword to search")))

(gptel-make-tool
 :category "web"
 :name "extract_page"
 :async t
 :function #'tavily-extract-async
 :description "Extract contents from a URL."
 :args '((:name "urls" :type array :description "Array of URLs to be fetched from")
         (:format "format" :type string :description "Either `text` or `markdown`")))

(gptel-make-tool
 :category "web"
 :name "fetch_url_text"
 :async t
 :description "Fetch the plaintext contents from a URL."
 :args '((:name "url" :type string :description "URL of the web page"))
 :function #'fetch-url-text-async)

(provide 'cys/gptel-tavily)
