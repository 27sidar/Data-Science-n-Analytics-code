SELECT 
    review.j->>'business_id' AS business_id,
    EXTRACT(YEAR FROM TO_TIMESTAMP(review.j->>'date', 'YYYY-MM-DD HH24:MI:SS')) AS review_year,
    AVG((review.j->>'stars')::FLOAT) AS avg_review_rating,
    SUM((review.j->>'useful')::INT) AS total_useful_votes,
    SUM((review.j->>'funny')::INT) AS total_funny_votes,
    SUM((review.j->>'cool')::INT) AS total_cool_votes,
    COUNT(*) AS total_reviews
FROM 
    public.review
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