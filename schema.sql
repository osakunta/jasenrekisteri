-- create schema jasen2;
-- set schema 'jasen2';

create table jasen2.credentials (
  username text not null primary key,
  password text not null
);

create sequence jasen2.event_id_seq;

create table jasen2.events (
  eid int not null default nextval('jasen2.event_id_seq') primary key,
  username text not null, -- username adder
  updated timestamp with time zone not null default current_timestamp,
  edata text not null
);
