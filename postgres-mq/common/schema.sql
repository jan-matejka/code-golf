create table public.queue(
  id serial not null primary key,
  ctime timestamp not null default now(),
  mtime timestamp null default null,
  data text not null
);
