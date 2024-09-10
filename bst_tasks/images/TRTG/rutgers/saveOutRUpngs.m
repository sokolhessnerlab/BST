for ind=1:numel(Rutgers4OnlineTrustIAT)
    switch(Rutgers4OnlineTrustIAT(ind).imgName(1:3))
        case 'M_C'
            prefix='white';
        case 'M_B'
            prefix='black';
        otherwise
            prefix='other';
    end
    newfname=sprintf('%s_sid%d_sk%d%d%d%d%d.png', prefix, Rutgers4OnlineTrustIAT(ind).id, Rutgers4OnlineTrustIAT(ind).skdata(1:5));
    imwrite(Rutgers4OnlineTrustIAT(ind).img, newfname, 'png');
end

