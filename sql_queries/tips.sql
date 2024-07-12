SELECT 
        tip.j->>'business_id' AS business_id,
        EXTRACT(YEAR FROM TO_TIMESTAMP(tip.j->>'date', 'YYYY-MM-DD HH24:MI:SS')) AS tip_year,
        COUNT(*) AS total_tips,
        SUM((tip.j->>'compliment_count')::INT) AS total_compliments
    FROM 
        public.tip tip
    WHERE 
        tip.j->>'business_id' IN (
            SELECT 
                business.j->>'business_id' 
            FROM 
                public.business
            WHERE 
                business.j->>'categories' LIKE '%Restaurant%'
        )
        AND DATE_PART('year', TO_TIMESTAMP(tip.j->>'date', 'YYYY-MM-DD HH24:MI:SS')) BETWEEN 2014 AND 2021
    GROUP BY 
        business_id, tip_year