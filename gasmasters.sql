--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.3
-- Dumped by pg_dump version 9.5.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: notify_reading_added(); Type: FUNCTION; Schema: public; Owner: gasmasters
--

CREATE FUNCTION notify_reading_added() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
PERFORM pg_notify('addedreading', CAST(NEW.tank AS TEXT));
RETURN NEW;
END;
$$;


ALTER FUNCTION public.notify_reading_added() OWNER TO gasmasters;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: hubs; Type: TABLE; Schema: public; Owner: gasmasters
--

CREATE TABLE hubs (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    lat double precision,
    lng double precision,
    vendor_id integer NOT NULL
);


ALTER TABLE hubs OWNER TO gasmasters;

--
-- Name: hubs_id_seq; Type: SEQUENCE; Schema: public; Owner: gasmasters
--

CREATE SEQUENCE hubs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE hubs_id_seq OWNER TO gasmasters;

--
-- Name: hubs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: gasmasters
--

ALTER SEQUENCE hubs_id_seq OWNED BY hubs.id;


--
-- Name: readings; Type: TABLE; Schema: public; Owner: gasmasters
--

CREATE TABLE readings (
    id integer NOT NULL,
    tank integer NOT NULL,
    dbreceived timestamp with time zone DEFAULT now(),
    sensorsent timestamp with time zone NOT NULL,
    value integer NOT NULL
);


ALTER TABLE readings OWNER TO gasmasters;

--
-- Name: readings_id_seq; Type: SEQUENCE; Schema: public; Owner: gasmasters
--

CREATE SEQUENCE readings_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE readings_id_seq OWNER TO gasmasters;

--
-- Name: readings_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: gasmasters
--

ALTER SEQUENCE readings_id_seq OWNED BY readings.id;


--
-- Name: tanks; Type: TABLE; Schema: public; Owner: gasmasters
--

CREATE TABLE tanks (
    id integer NOT NULL,
    hub integer NOT NULL,
    name character varying(255) NOT NULL,
    lng double precision NOT NULL,
    lat double precision NOT NULL,
    yellow_threshold integer DEFAULT 1024,
    red_threshold integer DEFAULT 512
);


ALTER TABLE tanks OWNER TO gasmasters;

--
-- Name: tanks_id_seq; Type: SEQUENCE; Schema: public; Owner: gasmasters
--

CREATE SEQUENCE tanks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tanks_id_seq OWNER TO gasmasters;

--
-- Name: tanks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: gasmasters
--

ALTER SEQUENCE tanks_id_seq OWNED BY tanks.id;


--
-- Name: vendors; Type: TABLE; Schema: public; Owner: gasmasters
--

CREATE TABLE vendors (
    id integer NOT NULL,
    name character varying(255),
    street character varying(255),
    city character varying(255),
    state character varying(2),
    zip character varying(10),
    username character varying(16),
    password bytea
);


ALTER TABLE vendors OWNER TO gasmasters;

--
-- Name: vendors_id_seq; Type: SEQUENCE; Schema: public; Owner: gasmasters
--

CREATE SEQUENCE vendors_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE vendors_id_seq OWNER TO gasmasters;

--
-- Name: vendors_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: gasmasters
--

ALTER SEQUENCE vendors_id_seq OWNED BY vendors.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY hubs ALTER COLUMN id SET DEFAULT nextval('hubs_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY readings ALTER COLUMN id SET DEFAULT nextval('readings_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY tanks ALTER COLUMN id SET DEFAULT nextval('tanks_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY vendors ALTER COLUMN id SET DEFAULT nextval('vendors_id_seq'::regclass);


--
-- Name: hubs_pkey; Type: CONSTRAINT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY hubs
    ADD CONSTRAINT hubs_pkey PRIMARY KEY (id);


--
-- Name: readings_pkey; Type: CONSTRAINT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY readings
    ADD CONSTRAINT readings_pkey PRIMARY KEY (id);


--
-- Name: tanks_pkey; Type: CONSTRAINT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY tanks
    ADD CONSTRAINT tanks_pkey PRIMARY KEY (id);


--
-- Name: unique_vendor_username; Type: CONSTRAINT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY vendors
    ADD CONSTRAINT unique_vendor_username UNIQUE (username);


--
-- Name: vendors_pkey; Type: CONSTRAINT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY vendors
    ADD CONSTRAINT vendors_pkey PRIMARY KEY (id);


--
-- Name: updated_readings_trigger; Type: TRIGGER; Schema: public; Owner: gasmasters
--

CREATE TRIGGER updated_readings_trigger AFTER INSERT ON readings FOR EACH ROW EXECUTE PROCEDURE notify_reading_added();


--
-- Name: fk_hub_vendor; Type: FK CONSTRAINT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY hubs
    ADD CONSTRAINT fk_hub_vendor FOREIGN KEY (vendor_id) REFERENCES vendors(id);


--
-- Name: fk_readings_tanks; Type: FK CONSTRAINT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY readings
    ADD CONSTRAINT fk_readings_tanks FOREIGN KEY (tank) REFERENCES tanks(id) ON DELETE CASCADE;


--
-- Name: fk_tanks_hubs; Type: FK CONSTRAINT; Schema: public; Owner: gasmasters
--

ALTER TABLE ONLY tanks
    ADD CONSTRAINT fk_tanks_hubs FOREIGN KEY (hub) REFERENCES hubs(id) ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

