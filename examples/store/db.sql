--
-- PostgreSQL database dump
--

-- Dumped from database version 15.3
-- Dumped by pg_dump version 15.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: customers; Type: TABLE; Schema: public; Owner: aguz
--

CREATE TABLE public.customers (
    id integer NOT NULL,
    name text NOT NULL,
    email text NOT NULL
);


ALTER TABLE public.customers OWNER TO aguz;

--
-- Name: customers_id_seq; Type: SEQUENCE; Schema: public; Owner: aguz
--

CREATE SEQUENCE public.customers_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.customers_id_seq OWNER TO aguz;

--
-- Name: customers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: aguz
--

ALTER SEQUENCE public.customers_id_seq OWNED BY public.customers.id;


--
-- Name: order_products; Type: TABLE; Schema: public; Owner: aguz
--

CREATE TABLE public.order_products (
    order_id integer NOT NULL,
    product_id integer NOT NULL,
    quantity integer DEFAULT 1 NOT NULL
);


ALTER TABLE public.order_products OWNER TO aguz;

--
-- Name: orders; Type: TABLE; Schema: public; Owner: aguz
--

CREATE TABLE public.orders (
    id integer NOT NULL,
    customer_id integer NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.orders OWNER TO aguz;

--
-- Name: orders_id_seq; Type: SEQUENCE; Schema: public; Owner: aguz
--

CREATE SEQUENCE public.orders_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.orders_id_seq OWNER TO aguz;

--
-- Name: orders_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: aguz
--

ALTER SEQUENCE public.orders_id_seq OWNED BY public.orders.id;


--
-- Name: products; Type: TABLE; Schema: public; Owner: aguz
--

CREATE TABLE public.products (
    id integer NOT NULL,
    name text NOT NULL,
    price numeric(10,2) NOT NULL
);


ALTER TABLE public.products OWNER TO aguz;

--
-- Name: products_id_seq; Type: SEQUENCE; Schema: public; Owner: aguz
--

CREATE SEQUENCE public.products_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.products_id_seq OWNER TO aguz;

--
-- Name: products_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: aguz
--

ALTER SEQUENCE public.products_id_seq OWNED BY public.products.id;


--
-- Name: customers id; Type: DEFAULT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.customers ALTER COLUMN id SET DEFAULT nextval('public.customers_id_seq'::regclass);


--
-- Name: orders id; Type: DEFAULT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.orders ALTER COLUMN id SET DEFAULT nextval('public.orders_id_seq'::regclass);


--
-- Name: products id; Type: DEFAULT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.products ALTER COLUMN id SET DEFAULT nextval('public.products_id_seq'::regclass);


--
-- Data for Name: customers; Type: TABLE DATA; Schema: public; Owner: aguz
--

COPY public.customers (id, name, email) FROM stdin;
1	John Doe	johndoe@example.com
2	Jane Smith	janesmith@example.com
3	Alice Johnson	alicejohnson@example.com
\.


--
-- Data for Name: order_products; Type: TABLE DATA; Schema: public; Owner: aguz
--

COPY public.order_products (order_id, product_id, quantity) FROM stdin;
1	1	2
1	2	1
2	2	3
3	3	5
3	1	1
4	4	2
4	5	1
5	6	1
5	7	2
5	8	3
\.


--
-- Data for Name: orders; Type: TABLE DATA; Schema: public; Owner: aguz
--

COPY public.orders (id, customer_id, created_at) FROM stdin;
1	1	2023-11-17 08:49:14.305389
2	2	2023-11-18 08:49:14.305389
3	3	2023-11-19 08:49:14.305389
4	1	2023-11-16 08:49:14.305389
5	2	2023-11-15 08:49:14.305389
\.


--
-- Data for Name: products; Type: TABLE DATA; Schema: public; Owner: aguz
--

COPY public.products (id, name, price) FROM stdin;
1	Smartphone X	699.99
2	Laptop Pro	1299.99
3	Wireless Earbuds	99.99
4	Smartwatch Series 5	249.99
5	Ultra HD TV	799.99
6	Gaming Console	499.99
7	Digital Camera	349.99
8	Bluetooth Speaker	59.99
9	Tablet Pro	399.99
10	Fitness Tracker	79.99
\.


--
-- Name: customers_id_seq; Type: SEQUENCE SET; Schema: public; Owner: aguz
--

SELECT pg_catalog.setval('public.customers_id_seq', 3, true);


--
-- Name: orders_id_seq; Type: SEQUENCE SET; Schema: public; Owner: aguz
--

SELECT pg_catalog.setval('public.orders_id_seq', 5, true);


--
-- Name: products_id_seq; Type: SEQUENCE SET; Schema: public; Owner: aguz
--

SELECT pg_catalog.setval('public.products_id_seq', 10, true);


--
-- Name: customers customers_pkey; Type: CONSTRAINT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.customers
    ADD CONSTRAINT customers_pkey PRIMARY KEY (id);


--
-- Name: order_products order_products_pkey; Type: CONSTRAINT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.order_products
    ADD CONSTRAINT order_products_pkey PRIMARY KEY (order_id, product_id);


--
-- Name: orders orders_pkey; Type: CONSTRAINT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.orders
    ADD CONSTRAINT orders_pkey PRIMARY KEY (id);


--
-- Name: products products_pkey; Type: CONSTRAINT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.products
    ADD CONSTRAINT products_pkey PRIMARY KEY (id);


--
-- Name: order_products_order_id_idx; Type: INDEX; Schema: public; Owner: aguz
--

CREATE INDEX order_products_order_id_idx ON public.order_products USING btree (order_id);


--
-- Name: order_products_product_id_idx; Type: INDEX; Schema: public; Owner: aguz
--

CREATE INDEX order_products_product_id_idx ON public.order_products USING btree (product_id);


--
-- Name: orders_customer_id_idx; Type: INDEX; Schema: public; Owner: aguz
--

CREATE INDEX orders_customer_id_idx ON public.orders USING btree (customer_id);


--
-- Name: order_products order_products_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.order_products
    ADD CONSTRAINT order_products_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.orders(id);


--
-- Name: order_products order_products_product_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.order_products
    ADD CONSTRAINT order_products_product_id_fkey FOREIGN KEY (product_id) REFERENCES public.products(id);


--
-- Name: orders orders_customer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: aguz
--

ALTER TABLE ONLY public.orders
    ADD CONSTRAINT orders_customer_id_fkey FOREIGN KEY (customer_id) REFERENCES public.customers(id);


--
-- PostgreSQL database dump complete
--

