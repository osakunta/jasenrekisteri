-- create schema jasen2;
-- set schema 'jasen2';

create table jasen2.credentials (
  email text not null primary key,
  username text not null
);

create table jasen2.tokencache (
  token text not null primary key,
  created timestamp with time zone not null  default current_timestamp,
  username text not null
);

create sequence jasen2.event_id_seq;

create table jasen2.events (
  eid int not null default nextval('jasen2.event_id_seq') primary key,
  username text not null, -- username adder
  updated timestamp with time zone not null default current_timestamp,
  edata text not null
);

create index events_mid_idx ON jasen2.events ((edata :: json ->> 'memberId'));
