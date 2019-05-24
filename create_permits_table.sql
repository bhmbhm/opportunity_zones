CREATE TABLE permits_test (
 account varchar(13),
 permit_id bigint DEFAULT 4,
 agency_id varchar(4),
 permit_status varchar(1),
 description varchar(50),
 state_class varchar(4),
 permit_type varchar(10),
 permit_type_dscr varchar(50),
 property_type varchar(3),
 issue_date varchar(10),
 year varchar(4),
 site_number varchar(9),
 site_pfx varchar(2),
 site_street_name varchar(50),
 site_tp varchar(4),
 site_sfx varchar(25),
 site_apt varchar(6)
);

\COPY permits_test FROM 'permits.csv' DELIMITER ',' CSV HEADER;