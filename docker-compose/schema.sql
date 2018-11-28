--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.5
-- Dumped by pg_dump version 9.6.5

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: ads; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE ads (
    id text NOT NULL,
    html text NOT NULL,
    political integer NOT NULL,
    not_political integer NOT NULL,
    title text NOT NULL,
    message text NOT NULL,
    thumbnail text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    lang text NOT NULL,
    images text[] NOT NULL,
    impressions integer DEFAULT 1 NOT NULL,
    political_probability double precision DEFAULT 0 NOT NULL,
    targeting text,
    suppressed boolean DEFAULT false NOT NULL,
    targets jsonb DEFAULT '[]'::jsonb,
    advertiser text,
    entities jsonb DEFAULT '[]'::jsonb,
    page text,
    lower_page character varying,
    targetings text[],
    paid_for_by text,
    targetedness integer,
    phash text[],
    listbuilding_fundraising_proba double precision
);


--
-- Name: partners; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE partners (
    id integer NOT NULL,
    email character varying DEFAULT ''::character varying NOT NULL,
    encrypted_password character varying DEFAULT ''::character varying NOT NULL,
    reset_password_token character varying,
    reset_password_sent_at timestamp without time zone,
    remember_created_at timestamp without time zone,
    sign_in_count integer DEFAULT 0 NOT NULL,
    current_sign_in_at timestamp without time zone,
    last_sign_in_at timestamp without time zone,
    current_sign_in_ip inet,
    last_sign_in_ip inet,
    failed_attempts integer DEFAULT 0 NOT NULL,
    unlock_token character varying,
    locked_at timestamp without time zone,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: partners_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE partners_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: partners_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE partners_id_seq OWNED BY partners.id;


--
-- Name: partners id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY partners ALTER COLUMN id SET DEFAULT nextval('partners_id_seq'::regclass);


--
-- Name: ads ads_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY ads
    ADD CONSTRAINT ads_pkey PRIMARY KEY (id);


--
-- Name: partners partners_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY partners
    ADD CONSTRAINT partners_pkey PRIMARY KEY (id);


--
-- Name: ads_lower_page_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ads_lower_page_idx ON ads USING btree (lower_page);


--
-- Name: index_ads_on_advertiser; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_advertiser ON ads USING btree (advertiser);


--
-- Name: index_ads_on_browser_lang; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_browser_lang ON ads USING btree (lang);


--
-- Name: index_ads_on_english_html; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_english_html ON ads USING gin (to_englishtsvector(html));


--
-- Name: index_ads_on_entities; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_entities ON ads USING gin (entities);


--
-- Name: index_ads_on_german_html; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_german_html ON ads USING gin (to_germantsvector(html));


--
-- Name: index_ads_on_lang; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_lang ON ads USING btree (lang);


--
-- Name: index_ads_on_page; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_page ON ads USING btree (page);


--
-- Name: index_ads_on_political_probability; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_political_probability ON ads USING btree (political_probability);


--
-- Name: index_ads_on_political_probability_lang_and_suppressed; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_political_probability_lang_and_suppressed ON ads USING btree (political_probability, lang, suppressed);


--
-- Name: index_ads_on_suppressed; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_suppressed ON ads USING btree (suppressed);


--
-- Name: index_ads_on_targets; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_ads_on_targets ON ads USING gin (targets);


--
-- Name: index_partners_on_email; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX index_partners_on_email ON partners USING btree (email);


--
-- Name: index_partners_on_reset_password_token; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX index_partners_on_reset_password_token ON partners USING btree (reset_password_token);


--
-- Name: index_partners_on_unlock_token; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX index_partners_on_unlock_token ON partners USING btree (unlock_token);


--
-- Name: ads notify_on_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER notify_on_update AFTER UPDATE ON ads FOR EACH ROW WHEN ((old.* IS DISTINCT FROM new.*)) EXECUTE PROCEDURE notify_update();


--
-- PostgreSQL database dump complete
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.5
-- Dumped by pg_dump version 9.6.5

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: __diesel_schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE __diesel_schema_migrations (
    version character varying(50) NOT NULL,
    run_on timestamp without time zone DEFAULT now() NOT NULL
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE schema_migrations (
    version character varying NOT NULL
);


--
-- Data for Name: __diesel_schema_migrations; Type: TABLE DATA; Schema: public; Owner: -
--

COPY __diesel_schema_migrations (version, run_on) FROM stdin;
20170710202056	2017-07-15 00:41:40.888016
20170714223830	2017-07-15 00:41:41.34283
20170717200852	2017-07-17 20:12:17.827834
20170718222122	2017-07-18 23:43:54.835826
20170719220514	2017-07-19 22:09:39.901932
20170808224202	2017-08-09 00:37:59.995078
00000000000000	2017-08-30 22:02:08.626514
20170829234524	2017-08-30 22:02:09.294515
20170829234532	2017-08-30 22:02:10.553827
20170831225230	2017-09-01 02:08:53.41052
20170831232451	2017-09-01 02:11:33.022516
20170901021623	2017-09-12 05:24:09.975253
20170912052059	2017-09-12 06:11:54.058515
20170919203139	2017-09-21 00:38:40.85312
20171010225338	2017-10-10 23:06:26.894515
20171010231026	2017-10-11 00:19:59.138805
20171011003921	2017-10-11 02:10:09.097989
20171020014535	2017-10-20 04:05:44.416154
20171031214629	2017-10-31 21:55:09.74066
20171031220657	2017-10-31 22:11:02.781429
20171110225355	2017-11-10 22:56:28.24252
20171120221501	2017-12-04 09:48:17.476367
20171128001041	2017-12-04 09:48:18.04067
20171128223433	2017-12-04 09:48:18.456751
20171128231853	2017-12-04 09:48:18.896115
20171129031418	2017-12-04 09:48:19.319251
20171130194741	2017-12-04 09:48:23.351846
20171130200329	2017-12-04 09:48:24.113258
20171201100800	2017-12-04 09:48:24.541395
20171201230804	2017-12-04 09:48:25.077788
20171204035636	2017-12-04 10:01:54.614529
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: -
--

COPY schema_migrations (version) FROM stdin;
20180329213144
20180404154741
20180605174158
20180711195118
20180508145211
20180508145223
20180508145229
20180508144909
20180508202031
20180508144918
20180515190833
20180515191728
20180908214700
20180630015303
20180717010112
20181001220231
\.


--
-- Name: __diesel_schema_migrations __diesel_schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY __diesel_schema_migrations
    ADD CONSTRAINT __diesel_schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: unique_schema_migrations; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX unique_schema_migrations ON schema_migrations USING btree (version);


--
-- PostgreSQL database dump complete
--

