SELECT 
    review.j->>'business_id' AS business_id,
    EXTRACT(YEAR FROM TO_TIMESTAMP(review.j->>'date', 'YYYY-MM-DD HH24:MI:SS')) AS review_year,
    SUM(CASE WHEN u.j->>'elite' IS NOT NULL THEN 1 ELSE 0 END) AS elite_reviews,
    SUM(CAST(u.j->>'fans' AS INTEGER)) AS n_fans_reviews,
	SUM(array_length(string_to_array(u.j->>'friends', ','), 1)) AS total_friends,
    AVG(CASE WHEN u.j->>'elite' IS NOT NULL THEN CAST(review.j->>'stars' AS FLOAT) ELSE NULL END) AS elite_stars_avg
FROM 
    public.review review
JOIN 
    public.users u ON review.j->>'user_id' = u.j->>'user_id'
WHERE 
    review.j->>'business_id' IN (
        SELECT 
            business.j->>'business_id' 
        FROM 
            public.business
        WHERE 
            business.j->>'categories' LIKE '%Restaurant%'
    )
    AND EXTRACT(YEAR FROM TO_TIMESTAMP(review.j->>'date', 'YYYY-MM-DD HH24:MI:SS')) BETWEEN 2014 AND 2021
GROUP BY 
    business_id, review_year