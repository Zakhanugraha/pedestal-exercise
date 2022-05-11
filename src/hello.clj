(ns hello          
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [clojure.data.json :as json]
            [io.pedestal.http.content-negotiation :as conneg]))

(defn ok [body]
  {:status 200 :body body
   :headers {"Content Type" "text/html"}})

(defn not-found []
  {:status 404 :body "Not found\n"})

(def unmentionables #{"YHWH" "Voldemort" "Mxyzptlk" "Rumplestiltskin" "曹操"})

(def supported-types ["text/html" "application/edn" "application/json" "text/plain"])

(def content-neg-intc (conneg/negotiate-content supported-types))

(defn greeting-for [nm]
  (cond
    (unmentionables nm) nil
    (empty? nm)         "Hello, world! \n"
    :else               (str "Hello, " nm "\n")))

(defn respond-hello [request]
  (let [nm   (get-in request [:query-params :name])
        resp (greeting-for nm)]
    (if resp
      (ok resp)
      (not-found))))

(def echo
  {:name ::echo
   :enter #(assoc % :response (ok (:request %)))})

(defn accepted-type
  [context]
  (get-in context [:request :accept :field] "text/plain"))

(defn transform-content
  [body content-type]
  (case content-type
    "text/html"        body
    "text/plain"       body
    "application/edn"  (pr-str body)
    "application/json" (json/write-str body)))

(defn coerce-to
  [response content-type]
  (-> response
      (update :body transform-content content-type)
      (assoc-in [:headers "Content-Type"] content-type)))

(def coerce-body
  {:name ::coerce-body
   :leave
   (fn [context]
     (cond-> context
       (nil? (get-in context [:response :headers "Content-Type"]))                    ;; <1>
       (update-in [:response] coerce-to (accepted-type context))))})

(def routes
  (route/expand-routes
   #{["/greet" :get [coerce-body content-neg-intc respond-hello] :route-name :greet]
     ["/echo"  :get echo]}))

(def server (atom nil))

(defn create-server []
  (http/create-server
   {::http/routes routes
    ::http/type   :jetty
    ::http/port   8890
    ::http/join?  false}))

(defn start []
  (swap! server
         (constantly (http/start (create-server))))
  nil) 

(defn stop []
  (swap! server http/stop)
  nil)