insert into events (name,start_date,end_date) values ('녹색지대',CURDATE(),CURDATE());
insert into university (name) values ('일반인'),('건국대학교');
UPDATE university
    JOIN events ON university.name = '건국대학교' AND events.name = '녹색지대'
    SET university.current_event = events.event_id
WHERE university.name = '건국대학교';
