--
-- PostgreSQL database dump
--

-- Dumped from database version 11.10 (Debian 11.10-0+deb10u1)
-- Dumped by pg_dump version 11.10 (Debian 11.10-0+deb10u1)

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

SET default_with_oids = false;

--
-- Name: kv; Type: TABLE; Schema: public; Owner: testdb
--

CREATE TABLE public.kv (
    k character varying(255) NOT NULL,
    v text
);


ALTER TABLE public.kv OWNER TO testdb;

--
-- Data for Name: kv; Type: TABLE DATA; Schema: public; Owner: testdb
--

COPY public.kv (k, v) FROM stdin;
\.


--
-- Name: kv kv_pkey; Type: CONSTRAINT; Schema: public; Owner: testdb
--

ALTER TABLE ONLY public.kv
    ADD CONSTRAINT kv_pkey PRIMARY KEY (k);


--
-- PostgreSQL database dump complete
--
