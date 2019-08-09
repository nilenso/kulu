(ns kulu-backend.invoices.api-test
  (:require [clojure.test :refer :all]
            [kulu-backend.invoices.api :as inv]
            [kulu-backend.organizations.model :as org]
            [kulu-backend.invoices.search :as inv-search]
            [kulu-backend.test-helper :refer :all]
            [kulu-backend.utils.api :as api-utils]
            [schema.core :as s]))


(use-fixtures :once (join-fixtures [set-test-env
                                    migrate-test-db
                                    silence-logging]))
(use-fixtures :each isolate-db)

(defn wait-until [done-fn? & {:keys [ms-per-loop timeout]
                              :or {ms-per-loop 1000 timeout 10000}}]
  (loop [elapsed (long 0)]
    (when-not (or (>= elapsed timeout) (done-fn?))
      (Thread/sleep ms-per-loop)
      (recur (long (+ elapsed ms-per-loop))))))

(deftest create-invoice
  (testing "POST create returns a valid schema"
    (s/validate inv/Invoice (api-utils/idiomatize-keys inv/create {:name "foobar"})))

  (testing "POST create returns the invoice id"
    (let [{:keys [id] :as invoice} (inv/create {:name "foobar"})]
      (is (= (count (keys invoice)) 1))
      (is ((comp not empty?) invoice)))))

(deftest index-of-invoices
  (testing "GET index returns a valid schema"
    (s/validate inv/PaginatedInvoiceList (api-utils/idiomatize-keys inv/index {})))

  (testing "GET index invoices returns all invoices"
    (let [org (org/store {:name "filenso"})]
      (dotimes [n 5] (inv/create {:name "bar" :organization-name "filenso"}))
      (is (= 5 (count (:items (inv/index {:organization-name "filenso"})))))))

  (testing "GET index invoices strips out private attrs"
    (inv/create {:storage-key "foo/bar.pdf"})
    (let [[i & _] (:items (inv/index {}))]
      (is ((comp not contains?) i :created-at))
      (is ((comp not contains?) i :updated-at)))))

(deftest lookup-invoice
  (testing "GET lookup returns a valid schema for an empty invoice"
    (let [{id :id} (inv/create {:storage-key "foo/bar.pdf"})]
      (s/validate inv/Invoice (api-utils/idiomatize-keys inv/lookup id))))

  (let [attrs {:storage-key "foo/invoice-12.pdf"
               :currency "INR"
               :amount (double 123)
               :date (java.sql.Date/valueOf "2014-08-23")
               :name "Invoice #12"}]

    (testing "GET lookup returns a valid schema for completely filled-in invoice"
      (let [{id :id} (inv/create attrs)]
        (s/validate inv/Invoice (api-utils/idiomatize-keys inv/lookup id))))

    (testing "GET lookup returns the correct invoice"
      (let [invoices-data [attrs {:name "Another invoice"}]
            [{id :id} _] (map inv/create invoices-data)
            {:keys [name date amount currency attachment-url]} (inv/lookup id)]
        (is (= (:name attrs) name))
        (is (= (:date attrs) date))
        (is (= (double (:amount attrs)) (double amount)))
        (is (= (:currency attrs) currency))))

    (testing "GET lookup strips out private attrs"
      (let [{id :id} (inv/create {:name "Flipkart"})
            i (inv/lookup id)]
        (is ((comp not contains?) i :updated-at))))

    (testing "GET lookup returns nil if the invoice is not found"
      (let [non-existent-id #uuid "fb857379-1b66-4499-b705-08ec68601c6c"]
        (is (nil? (inv/lookup non-existent-id)))))))

(deftest update-invoice
  (let [existing-attrs {:currency "INR"
                        :amount (double 123)
                        :date (java.sql.Date/valueOf "2014-02-23")
                        :name "Test Invoice #1"}
        new-attrs {:currency "USD"
                   :amount (double 9.99)
                   :date (java.sql.Date/valueOf "2014-02-13")
                   :name "Test Invoice #1 from Flipkart"}
        {id :id} (inv/create existing-attrs)]

    ;(testing "PUT updates the elasticsearch index"
    ;  (wait-until #(complement (nil?  (inv-search/lookup id))))
    ;  (inv/update [id {:name "New Name"}])
    ;  (wait-until #(= "New Name" (:name (:_source (inv-search/lookup id)))))
    ;  (let [{source :_source} (inv-search/lookup id)
    ;        {name :name} source]
    ;    (is (= name "New Name"))))

    (testing "PUT change with id returns a valid schema"
      (s/validate inv/Invoice
                  (api-utils/idiomatize-keys inv/update [id existing-attrs])))

    (testing "PUT updates the invoice with the given data"
      (let [{:keys [name date amount currency]} (inv/update [id new-attrs])]
        (is (= name (:name new-attrs)))
        (is (= date (:date new-attrs)))
        (is (= (double amount) (double (:amount new-attrs))))
        (is (= currency (:currency new-attrs)))))

    (testing "PUT also overwrites existing data with if the given value is nil"
      (let [new-attrs {:currency nil
                       :amount nil
                       :date nil
                       :name nil}]
        (let [{:keys [name date amount currency]} (inv/update [id new-attrs])]
          (is (= name nil))
          (is (= date nil))
          (is (= amount nil))
          (is (= currency nil)))))))

(deftest delete-invoice
  (testing "create an invoice, delete it, make sure it is gone"
    (let [id (java.util.UUID/randomUUID)
          created (api-utils/idiomatize-keys inv/create {:name "Tobee X. Terminated"
                                                          :id id})
          lookup-before (inv/lookup id)
          delete-result (inv/soft-delete id)
          lookup-after  (inv/lookup id)]
      (is (:id created))
      (is (map? lookup-before))
      (is (= id delete-result))
      (is (nil? lookup-after)))))
