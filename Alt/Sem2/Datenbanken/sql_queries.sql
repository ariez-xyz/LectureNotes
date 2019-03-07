-- QUERY 1
select distinct person.nickname 
	from person,posting 
	where person.nickname=posting.nickname
	order by person.nickname ASC;

-- QUERY 2
select count(nickname) as count
	from person 
	where birthdate > '1995-01-01';

-- QUERY 3
select location 
	from 
		posting
		inner join
			(select postnickname,postpostingid
				from likes 
				where postnickname='Bearbugar' 
				group by postnickname,postpostingid 
				having count(postpostingid) >= %(count)s
			) as ids 
			on nickname = postnickname and postingid = postpostingid
	order by location ASC;

-- QUERY 4
with tagCount as (
		select tag, count(tag) as count
			from tags
			where nickname=%(nickname)s
			group by tag
			order by count(tag) DESC
	)
	select tag,count
		from tagCount
		where count=(select max(count) from tagCount)
		order by tag ASC;

-- QUERY 5
select nickname,postingid
	from posting
	where (nickname,postingid) not in (select postnickname,postpostingid from likes)
	order by nickname ASC;

-- QUERY 6
--select nickname,firstname,lastname,birthdate
--	from person
--	where birthdate in (select birthdate from person
--				group by birthdate
--				order by birthdate DESC
--				LIMIT 1 OFFSET %(nth)s-1)
--	order by nickname;
with
	birthdates as
		(select distinct birthdate
			from person
			order by birthdate DESC
		),
	nthdate as
		(select birthdate 
			from birthdates
			limit 1
			offset %(nth)s-1
		)
	select nickname,firstname,lastname,birthdate
		from person
		where birthdate in (select * from nthdate);

-- QUERY 7
with 
	followcnt as 
		(select followeenickname as nickname, count(followeenickname) as followers
			from follows 
			group by followeenickname
		),
	matchingfollowcnt as
		(select nickname, followers 
			from followcnt
			where nickname like %(personpattern)s
		)

	select * 
		from matchingfollowcnt 
		where followers = (select min(followers) from matchingfollowcnt)
		order by nickname ASC;

-- QUERY 8
select distinct information.postingid,information.nickname
	from information
	inner join 
		(select nickname,postingid from information 
			inner join text on information.informationid=text.informationid
		) 
		as txtinfo 
			on 	txtinfo.nickname=information.nickname and 
				txtinfo.postingid=information.postingid 
	where informationid in (select informationid from data)
	order by nickname,postingid;

-- QUERY 9
select postingid,nickname,totalsize 
	from (
		select postingid,nickname,sum(size) as totalsize 
			from information 
			group by postingid,nickname 
			having sum(size) > %(size)s
		) as goodposts 
	where (nickname,postingid,%(tag)s) in (select * from tags)
	order by nickname ASC, postingid ASC;

-- QUERY 10
with
	numlikes as
		(select postnickname as nickname, postpostingid as postingid, count(postpostingid) as count
			from likes
			group by postnickname,postpostingid
		),
     	maxlikeposts as 
		(select * from numlikes where count=(select max(count) from numlikes))

	select distinct tag from tags,maxlikeposts
		where 	tags.nickname=maxlikeposts.nickname and 
			tags.postingid = maxlikeposts.postingid
		order by tag ASC;
