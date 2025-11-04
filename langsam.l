(defn type? [x] (= (type x) Type))

(defn nil? [x] (= x nil))
(defn some? [x] (not= x nil))

(defn boolean? [x] (= (type x) Boolean))
(defn integer? [x] (= (type x) Integer))
(defn float? [x] (= (type x) Float))
(defn string? [x] (= (type x) String))
(defn symbol? [x] (= (type x) Symbol))
(defn keyword? [x] (= (type x) Keyword))
(defn opword? [x] (= (type x) Opword))
(defn cons? [x] (= (type x) Cons))
(defn vector? [x] (= (type x) Vector))
(defn map? [x] (= (type x) Map))
(defn function? [x] (= (type x) Function))

(defn not [x]
  (if x false true))

(defn not= [x & rest]
  (not (apply = x rest)))

(defn special? [x]
  (and (function? x)
       (not (:evalargs x))
       (not (:evalresult x))))

(defmacro when [cond & body]
  `(if ,cond (do ,@body)))

(defmacro unless [cond & body]
  `(if (not ,cond) (do ,@body)))

(defn sublet [proto &opt (size 64)]
  (let [sublet (Map size)]
    (setproto sublet proto)
    sublet))

(defmacro import [name]
  `(def ,name (require ,(str name))))

(defn update [x k f & rest]
  (let [oldv (get x k)
        newv (apply f oldv rest)]
    (put x k newv)))

(defmacro comment [& body])

(def reset setcdr)

(defmacro setq [x val]
  `(reset (gep (curlet) ',x) ,val))

(defn swap [ref f & rest]
  (reset ref (apply f @ref rest)))

(defmacro swapq [x f & rest]
  `(swap (gep (curlet) ',x) ,f ,@rest))

(defn inc [x] (+ x 1))
(defmacro incq [x] `(swapq ,x ,inc))

(defn dec [x] (- x 1))
(defmacro decq [x] `(swapq ,x ,dec))

(defn even? [x] (= (mod x 2) 0))
(defn odd? [x] (not= (mod x 2) 0))

(defn map [f coll]
  (let [result nil
        it (iter coll)]
    (while it
      (setq result (cons (f @it) result))
      (setq it (next it)))
    (nreverse result)))

(defn reduce [f init coll]
  (let [result init
        it (iter coll)]
    (while it
      (setq result (f @it result))
      (setq it (next it)))
    result))

(defn filter [f coll]
  (let [result nil
        it (iter coll)]
    (while it
      (let [item @it]
        (if (f item)
          (setq result (cons item result))))
      (setq it (next it)))
    (nreverse result)))

(def gensym
  (let [counter 0]
    (fn gensym [&opt (prefix 'gensym)]
      (let [sym (Symbol (str prefix "#" counter))]
        (incq counter)
        sym))))

(defmacro and [x & rest]
  (if rest
    (let [val# (gensym 'val)]
      `(let [,val# ,x]
         (if ,val# (and ,@rest) ,val#)))
    x))

(defmacro or [x & rest]
  (if rest
    (let [val# (gensym 'val)]
      `(let [,val# ,x]
         (if ,val# ,val# (or ,@rest))))
    x))

(defmacro when-let [bindings & body]
  `(if-let ,bindings (do ,@body)))

(defmacro while-let [bindings & body]
  `(while (if-let ,bindings (do ,@body true) false)))

(def case
  (let [subcase (fn [item# clauses]
                  (if clauses
                    (if-let [[pattern result & clauses] clauses]
                      `(if-let [,pattern ,item#]
                         ,result
                         ,(subcase item# clauses))
                      (let [[result] clauses]
                        result))
                    `(throw (cons 'case "no match"))))]
    (macro case [item & clauses]
      (let [item# (gensym 'item)]
        `(let [,item# ,item]
           ,(subcase item# clauses))))))

(defmacro prog1 [head & rest]
  (let [result# (gensym 'result)]
    `(do (def ,result# ,head)
         ,@rest
         ,result#)))

(defmacro for [bindings & body]
  (if-let [[var coll & rest] bindings]
    (let [it# (gensym 'it)
          coll# (gensym 'coll)]
      `(let [,coll# ,coll
             ,it# (iter ,coll#)
             ,var nil]
         (while ,it#
           (setq ,var @,it#)
           (for ,rest ,@body)
           (setq ,it# (next ,it#)))))
    `(do ,@body)))

(defn group-by [keyfn coll]
  (let [groups {}
        it (iter coll)]
    (while it
      (let [item @it
            key (keyfn item)]
        (update groups key (fn [group item] (cons item group)) item))
      (setq it (next it)))
    groups))

(defn separate [pred coll]
  (let [left nil
        right nil
        it (iter coll)]
    (while it
      (let [item @it]
        (if (pred item)
          (setq left (cons item left))
          (setq right (cons item right))))
      (setq it (next it)))
    [(nreverse left) (nreverse right)]))

(defn iterable? [x]
  (if-let [it (iter x)]
    true false))

(defn flatten [coll]
  (def result nil)
  (for [item coll]
    (if (iterable? item)
      (for [x (flatten item)]
        (setq result (cons x result)))
      (setq result (cons item result))))
  ((type coll) (nreverse result)))

(defn flatten1 [coll]
  (def result nil)
  (for [item coll]
    (if (iterable? item)
      (for [x item]
        (setq result (cons x result)))
      (setq result (cons item result))))
  ((type coll) (nreverse result)))

(defmacro throws? [expected-payload & body]
  (let [catch-result# (gensym 'catch-result)]
    `(let [,catch-result# (catch [payload (cons %exception-payload payload)] ,@body)]
       (assert (= ,catch-result# (cons %exception-payload ,expected-payload))))))

(defmacro try [& body]
  (let [keyfn (fn [item] (if (cons? item)
                           (case (car item)
                             %catch :catch
                             %finally :finally
                             :body)
                           :body))
        {:keys [body catch finally]} (group-by keyfn body)
        finally-forms (flatten1 (map cdr finally))
        ex# (gensym 'ex)
        ex-patterns (map (fn [[_ pattern & body]]
                           `(,pattern (prog1 (do ,@body) ,@finally-forms)))
                      (nreverse catch))]
    `(catch [,ex# (case ,ex#
                    ,@(flatten1 ex-patterns)
                    (throw ,ex#))]
       ,@(nreverse body)
       ,@finally-forms)))

(defn macro-form?*
  [form env]
  (if-let [[(Symbol head) & rest] form]
    (macro? (eval head env))))

(defmacro macro-form?
  [form]
  `(macro-form?* ,form (curlet)))

(defn macroexpand*
  [form env]
  (if (macro-form?* form env)
    (macroexpand* (macroexpand-1 form env) env)
    form))

(defmacro macroexpand
  [form]
  `(macroexpand* ,form (curlet)))

(def macroexpand-all*
  (let [old-fn fn
        map-env (old-fn [f coll env]
                  (map (old-fn [x] (f x env)) coll))
        expand-qq (fn [form env]
                    (case form
                      (cons x y) (if (cons? y)
                                   (case x
                                     'unquote (cons 'unquote
                                                    (macroexpand-all* y env))
                                     'unquote-splicing (cons 'unquote-splicing
                                                             (macroexpand-all* y env))
                                     form)
                                   (cons (expand-qq x env)
                                         (expand-qq y env)))
                      (Vector x) (Vector (map-env expand-qq x env))
                      (Map x) (Map (map-env expand-qq x env))
                      form))]
    (fn macroexpand-all*
      [form env]
      (let [form (macroexpand* form env)]
        (case form
          (cons x y) (if (cons? y)
                       (case x
                         'quote form
                         'quasiquote (cons 'quasiquote (expand-qq y env))
                         (map-env macroexpand-all* form env))
                       (cons (macroexpand-all* x env) (macroexpand-all* y env)))
          (Vector x) (Vector (map-env macroexpand-all* x env))
          (Map x) (Map (map-env macroexpand-all* x env))
          form)))))

(defmacro macroexpand-all
  [form]
  `(macroexpand-all* ,form (curlet)))

(let [old-macro macro
      old-fn fn
      expand-body (old-fn [body env]
                    (map (old-fn [x] (macroexpand-all* x env)) body))]

  (setq macro
    (old-macro macro [& rest]
      (let [m# (gensym 'm)
            env# (gensym 'env)]
        `(let [,m# (,old-macro ,@rest)
               ,env# (getproto (curlet))]
           (update ,m# :body ,expand-body ,env#)))))

  (setq defmacro
    (old-macro defmacro [& rest]
      (let [m# (gensym 'm)
            name# (gensym 'name)
            env# (gensym 'env)]
        `(let [,m# (macro ,@rest)
               ,name# (:name ,m#)
               ,env# (getproto (curlet))]
           (if ,name#
             (put ,env# ,name# ,m#)
             (throw '(syntax . "missing macro name")))))))

  (setq fn
    (old-macro fn [& rest]
      (let [f# (gensym 'f)
            env# (gensym 'env)]
        `(let [,f# (,old-fn ,@rest)
               ,env# (getproto (curlet))]
           (update ,f# :body ,expand-body ,env#)))))

  (setq defn
    (old-macro defn [& rest]
      (let [f# (gensym 'f)
            name# (gensym 'name)
            env# (gensym 'env)]
        `(let [,f# (fn ,@rest)
               ,name# (:name ,f#)
               ,env# (getproto (curlet))]
           (if ,name#
             (put ,env# ,name# ,f#)
             (throw '(syntax . "missing function name"))))))))

(defmacro -> [head & rest]
  (case rest
    [[beg & end] & tail] `(-> (,beg ,head ,@end) ,@tail)
    [beg & tail] `(-> (,beg ,head) ,@tail)
    head))

(defmacro ->> [head & rest]
  (case rest
    [[beg & end] & tail] `(->> (,beg ,@end ,head) ,@tail)
    [beg & tail] `(->> (,beg ,head) ,@tail)
    head))

(defmacro with-syms [syms & body]
  (let [bindings (-> syms
                     (->> (map (fn [sym] [(Symbol (str sym "#")) `(gensym ',sym)])))
                     flatten1
                     Vector)]
    `(let ,bindings ,@body)))

(gc)
