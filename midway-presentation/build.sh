BUILD_DIR=_build
if [ ! -d $BUILD_DIR ];
then
    mkdir $BUILD_DIR;
fi
if [ ! -d $BUILD_DIR/images ];
then
    mkdir $BUILD_DIR/images;
fi

# cp images/* $BUILD_DIR/images

cp style.css $BUILD_DIR
pandoc -i -t beamer \
    -o presentation.tex \
    presentation.md
