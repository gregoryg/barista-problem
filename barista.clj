;; barista-o-matic (ns bom)
(ns barista-o-matic
  (:use clojure.contrib.pprint)
  (:gen-class))

(defn restock! []
  (def inventory
       ;; Inventory of raw ingredients. Store prices as cents and keep to integer math
       {
	:coffee {:name "Coffee" :count 10 :price 75}, 
	:decaf {:name "Decaf Coffee" :count 10 :price 75},
	:sugar {:name "Sugar" :count 10, :price 25}
	:cream {:name "Cream" :count 10 :price 25},
	:milk-steamed {:name "Steamed Milk" :count 10, :price 35},
	:milk-foamed {:name "Foamed Milk" :count 10, :price 35},
	:espresso {:name "Espresso" :desc "espresso shot", :count 10, :price 110},
	:cocoa {:name "Cocoa" :count 10, :price 90},
	:cream-whipped {:name "Whipped Cream" :count 10, :price 100}
	}))

(restock!)

(def menu
     {
      :house-coffee 
      {:menu-idx 1
       :menu-key :house-coffee 
       :name "House Coffee" 
       :desc "Real tasty AND popular",
       :ingredients {:coffee 3, :sugar 1, :cream 1}}, 
      :decaf-coffee
      {:menu-idx 2
       :menu-key :decaf-coffee
       :name "Decaf Coffee"
       :ingredients {:decaf 3, :sugar 1, :cream 1}},
      :caffe-latte
      {:menu-idx 3
       :menu-key :caffe-latte
       :name "Caffe Latte"
       :ingredients {:espresso 2, :milk-steamed 1}},
      :caffe-americano
      {:menu-idx 4
       :menu-key :caffe-americano
       :name "Caffe Americano"
       :ingredients {:espresso 3}},
      :caffe-mocha 
      {:menu-idx 5
       :menu-key :caffe-mocha 
       :name "Caffe Mocha" 
       :desc "Milk chocolate coffee",
       :ingredients {:espresso 3, :milk-steamed 2, :cocoa 3}},
      :cappucino
      {:menu-idx 6
       :menu-key :cappucino
       :name "Cappuccino"
       :ingredients {:espresso 2, :milk-steamed 1, :milk-foamed 1}}
      }
     )


(defn display-inventory 
  "Display the Barista-o-Matic inventory in the predefined output format"
  []
  (println "Inventory:")
  (doseq [[item {name :name, count :count}] inventory]
    (println (format "%s,%s" name  count))
  ))

(defn get-retail-price 
  "Return the price of the menu item (sum of all ingredients * units * per-unit-cost)"
  [menu-item]
	  (reduce +
		  (map #(* (:price (get inventory (key %))) (second %))
		       (seq (:ingredients menu-item)))))


(defn ingredients-in-stock? [menu-item-key]
  "Return true/false for each ingredient for the given menu item key"
  (let [ingredients (:ingredients (menu-item-key menu))]
    (merge-with <=
		ingredients
		(into {} (map #(into {} {% (get (get inventory %) :count)}) (keys ingredients))))))

(defn have-stock?
  "Given a menu item, return whether the ingredients are in stock"
  [menu-item-key]
  (every? true?
	  (vals
	   (ingredients-in-stock? menu-item-key))))

(every? true? (map second (merge-with <= {:espresso 3, :milk-steamed 2, :cocoa 2} {:espresso 10, :milk-steamed 1, :cocoa 5})))


(defn display-menu
  "Display the Barista-o-Matic menu"
  []
  (println "Menu:")
  (doseq [[menu-item-key {menu-idx :menu-idx, name :name, desc :desc, ingredients :ingredients}] menu]
    (println (format "%d,%s,$%.2f,%s"
		     menu-idx
		     name
		     (/ (get-retail-price (get menu menu-item-key)) 100.0)
		     (have-stock? menu-item-key)))))

(defn decrement-inventory
  "Return inventory map with given item reduced by count. Uses assoc-in for optimal awesomeness"
  [inventory-item-key count]
  (assoc-in inventory [inventory-item-key :count] (- (-> inventory inventory-item-key :count) count)))

(defn reduce-stock!
  "Side effect: replace value of inventory with given item reduced by count."
  [inventory-item-key count]
  (binding [*print-suppress-namespaces* true]
    (def inventory (decrement-inventory inventory-item-key count))))

(defn order-menu-item
  "Order a menu item by menu-idx"
  [menu-idx]
  (let [menu-item (first (filter #(= (:menu-idx %) menu-idx) (vals menu)))
	menu-key (:menu-key menu-item)
 	]
    (if (nil? menu-item)
      (print "No such menu item: " menu-idx "\n")
      (if (have-stock? menu-key)
	(do
	  (print "Dispensing " (:name menu-item) "...\n")
	  (map #(reduce-stock! (key %) (val %)) (:ingredients menu-item)))
	(print "Out of stock: " (:name menu-item) "\n")
      )
    )
  ))

